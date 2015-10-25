/* Copyright (C) 1995,1996,1997,1998,1999,2000,2001, 2003, 2004, 2005, 2006,
 *   2007, 2008, 2009, 2011, 2013, 2014 Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */




#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <stdio.h>
#include <errno.h>
#include <strftime.h>
#include <string.h>
#include <sys/times.h>
#include <sys/types.h>
#include <time.h>
#include <unistd.h>
#include <unistr.h>

#ifdef HAVE_SYS_TIMEB_H
# include <sys/timeb.h>
#endif

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/feature.h"
#include "libguile/strings.h"
#include "libguile/vectors.h"
#include "libguile/strings.h"

#include "libguile/validate.h"
#include "libguile/stime.h"




#if SCM_SIZEOF_LONG >= 8 && defined HAVE_CLOCK_GETTIME
/* Nanoseconds on 64-bit systems with POSIX timers.  */
#define TIME_UNITS_PER_SECOND 1000000000
#else
/* Milliseconds for everyone else.  */
#define TIME_UNITS_PER_SECOND 1000
#endif

long scm_c_time_units_per_second = TIME_UNITS_PER_SECOND;

static long
time_from_seconds_and_nanoseconds (long s, long ns)
{
  return s * TIME_UNITS_PER_SECOND
    + ns / (1000000000 / TIME_UNITS_PER_SECOND);
}


/* A runtime-selectable mechanism to choose a timing mechanism.  Really
   we want to use POSIX timers, but that's not always possible.  Notably,
   the user may have everything she needs at compile-time, but if she's
   running on an SMP machine without a common clock source, she can't
   use POSIX CPUTIME clocks.  */
static long (*get_internal_real_time) (void);
static long (*get_internal_run_time) (void);


#ifdef HAVE_CLOCK_GETTIME
struct timespec posix_real_time_base;

static long
get_internal_real_time_posix_timer (void)
{
  struct timespec ts;
  clock_gettime (CLOCK_REALTIME, &ts);
  return time_from_seconds_and_nanoseconds
    (ts.tv_sec - posix_real_time_base.tv_sec,
     ts.tv_nsec - posix_real_time_base.tv_nsec);
}

#if defined _POSIX_CPUTIME && defined CLOCK_PROCESS_CPUTIME_ID
/* You see, FreeBSD defines _POSIX_CPUTIME but not
   CLOCK_PROCESS_CPUTIME_ID.  */
#define HAVE_POSIX_CPUTIME 1

struct timespec posix_run_time_base;

static long
get_internal_run_time_posix_timer (void)
{
  struct timespec ts;
  clock_gettime (CLOCK_PROCESS_CPUTIME_ID, &ts);
  return time_from_seconds_and_nanoseconds
    (ts.tv_sec - posix_run_time_base.tv_sec,
     ts.tv_nsec - posix_run_time_base.tv_nsec);
}
#endif /* _POSIX_CPUTIME */
#endif /* HAVE_CLOCKTIME */
  
  
#ifdef HAVE_GETTIMEOFDAY
struct timeval gettimeofday_real_time_base;

static long
get_internal_real_time_gettimeofday (void)
{
  struct timeval tv;
  gettimeofday (&tv, NULL);
  return time_from_seconds_and_nanoseconds
    (tv.tv_sec - gettimeofday_real_time_base.tv_sec,
     (tv.tv_usec - gettimeofday_real_time_base.tv_usec) * 1000);
}
#endif


static long ticks_per_second;

static long
get_internal_run_time_times (void)
{
  struct tms time_buffer;
  times(&time_buffer);
  return (time_buffer.tms_utime + time_buffer.tms_stime)
    * TIME_UNITS_PER_SECOND / ticks_per_second;
}

static time_t fallback_real_time_base;
static long
get_internal_real_time_fallback (void)
{
  return time_from_seconds_and_nanoseconds
    ((long) time (NULL) - fallback_real_time_base, 0);
}


SCM_DEFINE (scm_get_internal_real_time, "get-internal-real-time", 0, 0, 0,
           (),
	    "Return the number of time units since the interpreter was\n"
	    "started.")
#define FUNC_NAME s_scm_get_internal_real_time
{
  return scm_from_long (get_internal_real_time ());
}
#undef FUNC_NAME


SCM_DEFINE (scm_times, "times", 0, 0, 0,
            (void),
	    "Return an object with information about real and processor\n"
	    "time.  The following procedures accept such an object as an\n"
	    "argument and return a selected component:\n"
	    "\n"
	    "@table @code\n"
	    "@item tms:clock\n"
	    "The current real time, expressed as time units relative to an\n"
	    "arbitrary base.\n"
	    "@item tms:utime\n"
	    "The CPU time units used by the calling process.\n"
	    "@item tms:stime\n"
	    "The CPU time units used by the system on behalf of the calling\n"
	    "process.\n"
	    "@item tms:cutime\n"
	    "The CPU time units used by terminated child processes of the\n"
	    "calling process, whose status has been collected (e.g., using\n"
	    "@code{waitpid}).\n"
	    "@item tms:cstime\n"
	    "Similarly, the CPU times units used by the system on behalf of\n"
	    "terminated child processes.\n"
	    "@end table")
#define FUNC_NAME s_scm_times
{
  struct tms t;
  clock_t rv;
  SCM factor;

  SCM result = scm_c_make_vector (5, SCM_UNDEFINED);
  rv = times (&t);
  if (rv == -1)
    SCM_SYSERROR;

  factor = scm_quotient (scm_from_long (TIME_UNITS_PER_SECOND),
                         scm_from_long (ticks_per_second));

  SCM_SIMPLE_VECTOR_SET (result, 0,
                         scm_product (scm_from_long (rv), factor));
  SCM_SIMPLE_VECTOR_SET (result, 1,
                         scm_product (scm_from_long (t.tms_utime), factor));
  SCM_SIMPLE_VECTOR_SET (result, 2,
                         scm_product (scm_from_long (t.tms_stime), factor));
  SCM_SIMPLE_VECTOR_SET (result ,3,
                         scm_product (scm_from_long (t.tms_cutime), factor));
  SCM_SIMPLE_VECTOR_SET (result, 4,
                         scm_product (scm_from_long (t.tms_cstime), factor));
  return result;
}
#undef FUNC_NAME

long
scm_c_get_internal_run_time (void)
{
  return get_internal_run_time ();
}

SCM_DEFINE (scm_get_internal_run_time, "get-internal-run-time", 0, 0, 0,
           (void),
	    "Return the number of time units of processor time used by the\n"
	    "interpreter.  Both @emph{system} and @emph{user} time are\n"
	    "included but subprocesses are not.")
#define FUNC_NAME s_scm_get_internal_run_time
{
  return scm_from_long (scm_c_get_internal_run_time ());
}
#undef FUNC_NAME

/* For reference, note that current-time and gettimeofday both should be
   protected against setzone/restorezone changes in another thread, since on
   DOS the system time is normally kept as local time, which means TZ
   affects the return from current-time and gettimeofday.  Not sure if DJGPP
   etc actually has concurrent multi-threading, but it seems prudent not to
   make assumptions about this.  */

SCM_DEFINE (scm_current_time, "current-time", 0, 0, 0,
           (void),
	    "Return the number of seconds since 1970-01-01 00:00:00 UTC,\n"
	    "excluding leap seconds.")
#define FUNC_NAME s_scm_current_time
{
  time_t timv;

  SCM_CRITICAL_SECTION_START;
  timv = time (NULL);
  SCM_CRITICAL_SECTION_END;
  if (timv == -1)
    SCM_MISC_ERROR ("current time not available", SCM_EOL);
  return scm_from_long (timv);
}
#undef FUNC_NAME

SCM_DEFINE (scm_gettimeofday, "gettimeofday", 0, 0, 0,
            (void),
	    "Return a pair containing the number of seconds and microseconds\n"
	    "since 1970-01-01 00:00:00 UTC, excluding leap seconds.  Note:\n"
	    "whether true microsecond resolution is available depends on the\n"
	    "operating system.")
#define FUNC_NAME s_scm_gettimeofday
{
#ifdef HAVE_GETTIMEOFDAY
  struct timeval time;

  if (gettimeofday (&time, NULL))
    SCM_SYSERROR;
  
  return scm_cons (scm_from_long (time.tv_sec),
		   scm_from_long (time.tv_usec));
#else
  time_t t = time (NULL);
  if (errno)
    SCM_SYSERROR;
  else
    return scm_cons (scm_from_long ((long)t), SCM_INUM0);
#endif
}
#undef FUNC_NAME

static SCM
scm_from_struct_tm (struct tm *tm, int zoff, SCM zone)
{
  SCM result = scm_c_make_vector (11, SCM_UNDEFINED);

  SCM_SIMPLE_VECTOR_SET (result, 0, scm_from_int (tm->tm_sec));
  SCM_SIMPLE_VECTOR_SET (result, 1, scm_from_int (tm->tm_min));
  SCM_SIMPLE_VECTOR_SET (result, 2, scm_from_int (tm->tm_hour));
  SCM_SIMPLE_VECTOR_SET (result, 3, scm_from_int (tm->tm_mday));
  SCM_SIMPLE_VECTOR_SET (result, 4, scm_from_int (tm->tm_mon));
  SCM_SIMPLE_VECTOR_SET (result, 5, scm_from_int (tm->tm_year));
  SCM_SIMPLE_VECTOR_SET (result, 6, scm_from_int (tm->tm_wday));
  SCM_SIMPLE_VECTOR_SET (result, 7, scm_from_int (tm->tm_yday));
  SCM_SIMPLE_VECTOR_SET (result, 8, scm_from_int (tm->tm_isdst));
  SCM_SIMPLE_VECTOR_SET (result, 9, scm_from_int (zoff));
  SCM_SIMPLE_VECTOR_SET (result, 10, zone);

  return result;
}

static timezone_t
scm_to_timezone (SCM zone, const char *FUNC_NAME)
{
  timezone_t tz;

  if (SCM_UNBNDP (zone))
    /* A NULL argument to tzalloc requests UTC.  */
    tz = tzalloc (getenv ("TZ"));
  else
    {
      char *zone_str = scm_to_locale_string (zone);
      tz = tzalloc (zone_str);
      free (zone_str);
    }

  if (!tz)
    SCM_SYSERROR;

  return tz;
}

static SCM
scm_struct_tm_zone_name (struct tm *tm)
{
#if defined HAVE_STRUCT_TM_TM_ZONE
  return scm_from_locale_string (tm->tm_zone);
#elif defined HAVE_TZNAME
  return scm_from_locale_string (tzname[ (tm->tm_isdst == 1) ? 1 : 0 ]);
#else
  return SCM_BOOL_F;
#endif
}

/* copy time components from a Scheme object to a struct tm.  */
static void
scm_to_struct_tm_and_timezone (SCM sbd_time, struct tm *tm, timezone_t *tz,
                               int pos, const char *subr)
{
  SCM_ASSERT (scm_is_vector (sbd_time)
	      && SCM_SIMPLE_VECTOR_LENGTH (sbd_time) == 11,
	      sbd_time, pos, subr);

  tm->tm_sec = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 0));
  tm->tm_min = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 1));
  tm->tm_hour = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 2));
  tm->tm_mday = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 3));
  tm->tm_mon = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 4));
  tm->tm_year = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 5));
  tm->tm_wday = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 6));
  tm->tm_yday = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 7));
  tm->tm_isdst = scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 8));
#if HAVE_STRUCT_TM_TM_GMTOFF
  tm->tm_gmtoff = - scm_to_int (SCM_SIMPLE_VECTOR_REF (sbd_time, 9));
#endif
#ifdef HAVE_STRUCT_TM_TM_ZONE
  tm->tm_zone = "";
#endif

  if (tz)
    {
      SCM zone = SCM_SIMPLE_VECTOR_REF (sbd_time, 10);
      /* If the time zone is false, default to the current TZ.  */
      *tz = scm_to_timezone (scm_is_true (zone) ? zone : SCM_UNDEFINED,
                             subr);
    }
}

SCM_DEFINE (scm_localtime, "localtime", 1, 1, 0,
            (SCM time, SCM zone),
	    "Return an object representing the broken down components of\n"
	    "@var{time}, an integer like the one returned by\n"
	    "@code{current-time}.  The time zone for the calculation is\n"
	    "optionally specified by @var{zone} (a string), otherwise the\n"
	    "@code{TZ} environment variable or the system default is used.")
#define FUNC_NAME s_scm_localtime
{
  time_t itime;
  struct tm lt, utc;
  timezone_t tz;
  SCM zone_name, result;
  int zoff;

  itime = SCM_NUM2LONG (1, time);

  tz = scm_to_timezone (zone, FUNC_NAME);
  if (!localtime_rz (tz, &itime, &lt))
    {
      int saved_errno = errno;
      tzfree (tz);
      errno = saved_errno;
      SCM_SYSERROR;
    }

  zone_name = scm_struct_tm_zone_name (&lt);
  tzfree (tz);

  if (!gmtime_r (&itime, &utc))
    SCM_SYSERROR;

  /* calculate timezone offset in seconds west of UTC.  */
  zoff = (utc.tm_hour - lt.tm_hour) * 3600 + (utc.tm_min - lt.tm_min) * 60
    + utc.tm_sec - lt.tm_sec;
  if (utc.tm_year < lt.tm_year)
    zoff -= 24 * 60 * 60;
  else if (utc.tm_year > lt.tm_year)
    zoff += 24 * 60 * 60;
  else if (utc.tm_yday < lt.tm_yday)
    zoff -= 24 * 60 * 60;
  else if (utc.tm_yday > lt.tm_yday)
    zoff += 24 * 60 * 60;

  result = scm_from_struct_tm (&lt, zoff, zone_name);

  return result;
}
#undef FUNC_NAME

SCM_DEFINE (scm_gmtime, "gmtime", 1, 0, 0,
            (SCM time),
	    "Return an object representing the broken down components of\n"
	    "@var{time}, an integer like the one returned by\n"
	    "@code{current-time}.  The values are calculated for UTC.")
#define FUNC_NAME s_scm_gmtime
{
  time_t itime;
  struct tm utc;
  SCM zone_name;

  itime = SCM_NUM2LONG (1, time);

  if (!gmtime_r (&itime, &utc))
    SCM_SYSERROR;

  zone_name = scm_struct_tm_zone_name (&utc);
  if (scm_is_false (zone_name))
    zone_name = scm_from_latin1_string ("GMT");

  return scm_from_struct_tm (&utc, 0, zone_name);
}
#undef FUNC_NAME

SCM_DEFINE (scm_mktime, "mktime", 1, 1, 0,
            (SCM sbd_time, SCM zone),
	    "@var{sbd_time} is an object representing broken down time and\n"
	    "@code{zone} is an optional time zone specifier (otherwise the\n"
	    "TZ environment variable or the system default is used).\n"
	    "\n"
	    "Returns a pair: the car is a corresponding integer time value\n"
	    "like that returned by @code{current-time}; the cdr is a broken\n"
	    "down time object, similar to as @var{sbd_time} but with\n"
	    "normalized values.")
#define FUNC_NAME s_scm_mktime
{
  time_t itime;
  struct tm lt, utc;
  timezone_t tz;
  SCM zone_name, result;
  int zoff;

  scm_to_struct_tm_and_timezone (sbd_time, &lt, NULL, SCM_ARG1, FUNC_NAME);
  tz = scm_to_timezone (zone, FUNC_NAME);

  itime = mktime_z (tz, &lt);
  if (itime == -1)
    {
      int errno_save = errno;
      tzfree (tz);
      errno = errno_save;
      SCM_SYSERROR;
    }

  zone_name = scm_struct_tm_zone_name (&lt);
  tzfree (tz);

  /* get timezone offset in seconds west of UTC.  */
  errno = EINVAL;
  if (!gmtime_r (&itime, &utc))
    SCM_SYSERROR;

  zoff = (utc.tm_hour - lt.tm_hour) * 3600 + (utc.tm_min - lt.tm_min) * 60
    + utc.tm_sec - lt.tm_sec;
  if (utc.tm_year < lt.tm_year)
    zoff -= 24 * 60 * 60;
  else if (utc.tm_year > lt.tm_year)
    zoff += 24 * 60 * 60;
  else if (utc.tm_yday < lt.tm_yday)
    zoff -= 24 * 60 * 60;
  else if (utc.tm_yday > lt.tm_yday)
    zoff += 24 * 60 * 60;

  result = scm_cons (scm_from_long (itime),
		     scm_from_struct_tm (&lt, zoff, zone_name));

  return result;
}
#undef FUNC_NAME

#ifdef HAVE_TZSET
SCM_DEFINE (scm_tzset, "tzset", 0, 0, 0,
            (void),
	    "Initialize the timezone from the TZ environment variable\n"
	    "or the system default.  It's not usually necessary to call this procedure\n"
	    "since it's done automatically by other procedures that depend on the\n"
	    "timezone.")
#define FUNC_NAME s_scm_tzset
{
  tzset();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif /* HAVE_TZSET */

SCM_DEFINE (scm_strftime, "strftime", 2, 0, 0,
            (SCM format, SCM stime),
	    "Return a string which is broken-down time structure @var{stime}\n"
	    "formatted according to the given @var{format} string.\n"
	    "\n"
	    "@var{format} contains field specifications introduced by a\n"
	    "@samp{%} character.  See @ref{Formatting Calendar Time,,, libc,\n"
	    "The GNU C Library Reference Manual}, or @samp{man 3 strftime},\n"
	    "for the available formatting.\n"
	    "\n"
	    "@lisp\n"
	    "(strftime \"%c\" (localtime (current-time)))\n"
	    "@result{} \"Mon Mar 11 20:17:43 2002\"\n"
	    "@end lisp\n"
	    "\n"
	    "If @code{setlocale} has been called (@pxref{Locales}), month\n"
	    "and day names are from the current locale and in the locale\n"
	    "character set.")
#define FUNC_NAME s_scm_strftime
{
  struct tm t;
  timezone_t tz;
  char *fmt;
  size_t size, written;
  SCM result;

  SCM_VALIDATE_STRING (1, format);
  scm_to_struct_tm_and_timezone (stime, &t, &tz, SCM_ARG2, FUNC_NAME);

  /* Sadly, Gnulib's nstrftime can't or won't extract the zone name from
     the timezone_t, so we have to make sure the tm_zone is set as
     well.  */
#ifdef HAVE_STRUCT_TM_TM_ZONE
  {
    SCM zone = SCM_SIMPLE_VECTOR_REF (stime, 10);
    t.tm_zone = scm_is_true (zone) ? scm_to_locale_string (zone) : NULL;
  }
#endif

  /* Convert string to UTF-8 so that non-ASCII characters in the
     format are passed through unchanged.  */
  fmt = scm_to_utf8_string (format);

  /* Use `nstrftime ()' from Gnulib, which supports all GNU extensions
     supported by glibc.  The first call is to compute how many bytes
     are needed in the buffer; the second actually writes them into the
     buffer.  */
  size = nstrftime (NULL, (size_t)-1, fmt, &t, NULL, 0);
  if (size)
    {
      char *buf = scm_malloc (size + 1);
      written = nstrftime (buf, size + 1, fmt, &t, NULL, 0);
      if (written != size)
        abort ();
      result = scm_from_utf8_string (buf);
      free (buf);
    }
  else
    result = scm_from_latin1_string ("");

  tzfree (tz);
  free (fmt);
#ifdef HAVE_STRUCT_TM_TM_ZONE
  free ((char *) t.tm_zone);
#endif

  return result;
}
#undef FUNC_NAME

#ifdef HAVE_STRPTIME
SCM_DEFINE (scm_strptime, "strptime", 2, 0, 0,
            (SCM format, SCM string),
	    "Performs the reverse action to @code{strftime}, parsing\n"
	    "@var{string} according to the specification supplied in\n"
	    "@var{format}.  The interpretation of month and day names is\n"
	    "dependent on the current locale.  The value returned is a pair.\n"
	    "The car has an object with time components\n"
	    "in the form returned by @code{localtime} or @code{gmtime},\n"
	    "but the time zone components\n"
	    "are not usefully set.\n"
	    "The cdr reports the number of characters from @var{string}\n"
	    "which were used for the conversion.")
#define FUNC_NAME s_scm_strptime
{
  struct tm t;
  char *fmt, *str, *rest;
  size_t used_len;
  long zoff;

  SCM_VALIDATE_STRING (1, format);
  SCM_VALIDATE_STRING (2, string);

  /* Convert strings to UTF-8 so that non-ASCII characters are passed
     through unchanged.  */
  fmt = scm_to_utf8_string (format);
  str = scm_to_utf8_string (string);

  /* initialize the struct tm */
#define tm_init(field) t.field = 0
  tm_init (tm_sec);
  tm_init (tm_min);
  tm_init (tm_hour);
  tm_init (tm_mday);
  tm_init (tm_mon);
  tm_init (tm_year);
  tm_init (tm_wday);
  tm_init (tm_yday);
#if HAVE_STRUCT_TM_TM_GMTOFF
  tm_init (tm_gmtoff);
#endif
#undef tm_init

  /* GNU glibc strptime() "%s" is affected by the current timezone, since it
     reads a UTC time_t value and converts with localtime_r() to set the tm
     fields, hence the use of SCM_CRITICAL_SECTION_START.  */
  t.tm_isdst = -1;
  SCM_CRITICAL_SECTION_START;
  rest = strptime (str, fmt, &t);
  SCM_CRITICAL_SECTION_END;
  if (rest == NULL)
    {
      /* POSIX doesn't say strptime sets errno, and on glibc 2.3.2 for
         instance it doesn't.  Force a sensible value for our error
         message.  */
      errno = EINVAL;
      scm_remember_upto_here_2 (format, string);
      free (str);
      free (fmt);
      SCM_SYSERROR;
    }

  /* tm_gmtoff is set by GNU glibc strptime "%s", so capture it when
     available */
#if HAVE_STRUCT_TM_TM_GMTOFF
  zoff = - t.tm_gmtoff;  /* seconds west, not east */
#else
  zoff = 0;
#endif

  /* Compute the number of UTF-8 characters.  */
  used_len = u8_strnlen ((scm_t_uint8*) str, rest-str);
  scm_remember_upto_here_2 (format, string);
  free (str);
  free (fmt);

  return scm_cons (scm_from_struct_tm (&t, zoff, SCM_BOOL_F),
		   scm_from_signed_integer (used_len));
}
#undef FUNC_NAME
#endif /* HAVE_STRPTIME */

void
scm_init_stime()
{
  scm_c_define ("internal-time-units-per-second",
		scm_from_long (SCM_TIME_UNITS_PER_SECOND));

  /* Init POSIX timers, and see if we can use them. */
#ifdef HAVE_CLOCK_GETTIME
  if (clock_gettime (CLOCK_REALTIME, &posix_real_time_base) == 0)
    get_internal_real_time = get_internal_real_time_posix_timer;

#ifdef HAVE_POSIX_CPUTIME
  {
    clockid_t dummy;
    
    /* Only use the _POSIX_CPUTIME clock if it's going to work across
       CPUs. */
    if (clock_getcpuclockid (0, &dummy) == 0 &&
        clock_gettime (CLOCK_PROCESS_CPUTIME_ID, &posix_run_time_base) == 0)
      get_internal_run_time = get_internal_run_time_posix_timer;
    else
      errno = 0;
  }
#endif /* HAVE_POSIX_CPUTIME */
#endif /* HAVE_CLOCKTIME */

  /* If needed, init and use gettimeofday timer. */
#ifdef HAVE_GETTIMEOFDAY
  if (!get_internal_real_time
      && gettimeofday (&gettimeofday_real_time_base, NULL) == 0)
    get_internal_real_time = get_internal_real_time_gettimeofday;
#endif

  /* Init ticks_per_second for scm_times, and use times(2)-based
     run-time timer if needed. */
#ifdef _SC_CLK_TCK
  ticks_per_second = sysconf (_SC_CLK_TCK);
#else
  ticks_per_second = CLK_TCK;
#endif
  if (!get_internal_run_time)
    get_internal_run_time = get_internal_run_time_times;

  if (!get_internal_real_time)
    /* No POSIX timers, gettimeofday doesn't work... badness!  */
    {
      fallback_real_time_base = time (NULL);
      get_internal_real_time = get_internal_real_time_fallback;
    }

  scm_add_feature ("current-time");
#include "libguile/stime.x"
}


/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
