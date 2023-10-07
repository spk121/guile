/* Copyright 1995-2002,2004,2006-2009,2011,2013-2014,2017-2018,2023
     Free Software Foundation, Inc.

   This file is part of Guile.

   Guile is free software: you can redistribute it and/or modify it
   under the terms of the GNU Lesser General Public License as published
   by the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Guile is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
   License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with Guile.  If not, see
   <https://www.gnu.org/licenses/>.  */




#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <fcntl.h>      /* for mingw */
#include <signal.h>
#include <stdio.h>
#include <errno.h>

#ifdef HAVE_PROCESS_H
#include <process.h>    /* for mingw */
#endif

#include <unistd.h>

#ifdef HAVE_SYS_TIME_H
#include <sys/time.h>
#endif

#include <full-write.h>

#include "async.h"
#include "boolean.h"
#include "dynwind.h"
#include "eval.h"
#include "feature.h"
#include "gsubr.h"
#include "list.h"
#include "modules.h"
#include "numbers.h"
#include "pairs.h"
#include "procs.h"
#include "syscalls.h"
#include "threads.h"
#include "variable.h"
#include "vectors.h"

#include "scmsigs.h"




/* take_signal is installed as the C signal handler whenever a Scheme
   handler is set.  When a signal arrives, take_signal will write a
   byte into the 'signal pipe'.  The 'signal delivery thread' will
   read this pipe and queue the appropriate asyncs.

   When Guile is built without threads, the signal handler will
   install the async directly.
*/


/* Scheme vectors with information about a signal.  signal_handlers
   contains the handler procedure or #f, signal_handler_asyncs
   contains the thunk to be marked as an async when the signal arrives
   (or the cell with the thunk in a singlethreaded Guile), and
   signal_handler_threads points to the thread that a signal should be
   delivered to.
*/
static scm_i_pthread_mutex_t signal_handler_lock =
  SCM_I_PTHREAD_MUTEX_INITIALIZER;
static SCM *signal_handlers;
static SCM signal_handler_asyncs;
static SCM signal_handler_threads;

/* The signal delivery thread.  */
scm_thread *scm_i_signal_delivery_thread = NULL;

/* The mutex held when launching the signal delivery thread.  */
static scm_i_pthread_mutex_t signal_delivery_thread_mutex =
  SCM_I_PTHREAD_MUTEX_INITIALIZER;


/* saves the original C handlers, when a new handler is installed.
   set to SIG_ERR if the original handler is installed.  */
#ifdef HAVE_SIGACTION
static struct sigaction orig_handlers[NSIG];
#else
static void (*orig_handlers[NSIG])(int);
#endif

static SCM
close_1 (SCM proc, SCM arg)
{
  /* Eval in the root module so that `lambda' has its usual meaning.  */
  return scm_eval (scm_list_3 (scm_sym_lambda, SCM_EOL,
                               scm_list_2 (proc, arg)),
                   scm_the_root_module ());
}

#if SCM_USE_PTHREAD_THREADS
/* On mingw there's no notion of inter-process signals, only a raise()
   within the process itself which apparently invokes the registered handler
   immediately.  Not sure how well the following code will cope in this
   case.  It builds but it may not offer quite the same scheme-level
   semantics as on a proper system.  If you're relying on much in the way of
   signal handling on mingw you probably lose anyway.  */

static int signal_pipe[2];

static void
take_signal (int signum)
{
  int old_errno = errno;
  char sigbyte = signum;
  full_write (signal_pipe[1], &sigbyte, 1);

#ifndef HAVE_SIGACTION
  signal (signum, take_signal);
#endif
  errno = old_errno;
}

struct signal_pipe_data
{
  char sigbyte;
  ssize_t n;
  int err;
};

static void*
read_signal_pipe_data (void * data)
{
  struct signal_pipe_data *sdata = data;
  
  sdata->n = read (signal_pipe[0], &sdata->sigbyte, 1);
  sdata->err = errno;

  return NULL;
}
  
static SCM
signal_delivery_thread (void *data)
{
  int sig;
#if HAVE_PTHREAD_SIGMASK  /* not on mingw, see notes above */
  sigset_t all_sigs;
  sigfillset (&all_sigs);
  /* On libgc 7.1 and earlier, GC_do_blocking doesn't actually do
     anything.  So in that case, libgc will want to suspend the signal
     delivery thread, so we need to allow it to do so by unmasking the
     suspend signal.  */
  sigdelset (&all_sigs, GC_get_suspend_signal ());
  scm_i_pthread_sigmask (SIG_SETMASK, &all_sigs, NULL);
#endif

  while (1)
    {
      struct signal_pipe_data sigdata;

      /* This tick gives any pending asyncs a chance to run before we
         block indefinitely waiting for a signal to arrive.  For example
         it can happen that the garbage collector is triggered while
         marking the signal handler for future execution.  Due to the
         way the after-gc-hook is designed, without a call to
         scm_async_tick, the after-gc-hook will not be triggered. */
      scm_async_tick ();

      scm_without_guile (read_signal_pipe_data, &sigdata);
      
      sig = sigdata.sigbyte;
      if (sigdata.n == 1 && sig >= 0 && sig < NSIG)
	{
	  SCM h, t;

	  h = SCM_SIMPLE_VECTOR_REF (signal_handler_asyncs, sig);
	  t = SCM_SIMPLE_VECTOR_REF (signal_handler_threads, sig);
	  if (scm_is_true (h))
	    scm_system_async_mark_for_thread (h, t);
	}
      else if (sigdata.n == 0)
	break; /* the signal pipe was closed. */
      else if (sigdata.n < 0 && sigdata.err != EINTR)
	perror ("error in signal delivery thread");
    }

  return SCM_UNSPECIFIED; /* not reached unless all other threads exited */
}

static void
start_signal_delivery_thread (void)
{
  SCM signal_thread;

  scm_i_pthread_mutex_lock (&signal_delivery_thread_mutex);

  if (pipe2 (signal_pipe, O_CLOEXEC) != 0)
    scm_syserror (NULL);
  signal_thread = scm_spawn_thread (signal_delivery_thread, NULL,
				    scm_handle_by_message,
				    "signal delivery thread");
  scm_i_signal_delivery_thread = SCM_I_THREAD_DATA (signal_thread);

  scm_i_pthread_mutex_unlock (&signal_delivery_thread_mutex);
}

void
scm_i_ensure_signal_delivery_thread ()
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, start_signal_delivery_thread);
}

#else /* !SCM_USE_PTHREAD_THREADS */

static void
take_signal (int signum)
{
  SCM cell = SCM_SIMPLE_VECTOR_REF (signal_handler_asyncs, signum);
  scm_thread *t = SCM_I_CURRENT_THREAD;

  if (scm_is_false (SCM_CDR (cell)))
    {
      SCM_SETCDR (cell, t->pending_asyncs);
      t->pending_asyncs = cell;
    }

#ifndef HAVE_SIGACTION
  signal (signum, take_signal);
#endif
}

void
scm_i_ensure_signal_delivery_thread ()
{
  return;
}

#endif /* !SCM_USE_PTHREAD_THREADS */

static void
install_handler (int signum, SCM thread, SCM handler)
{
  if (scm_is_false (handler))
    {
      SCM_SIMPLE_VECTOR_SET (*signal_handlers, signum, SCM_BOOL_F);
      SCM_SIMPLE_VECTOR_SET (signal_handler_asyncs, signum, SCM_BOOL_F);
    }
  else
    {
      SCM async = close_1 (handler, scm_from_int (signum));
#if !SCM_USE_PTHREAD_THREADS
      async = scm_cons (async, SCM_BOOL_F);
#endif
      SCM_SIMPLE_VECTOR_SET (*signal_handlers, signum, handler);
      SCM_SIMPLE_VECTOR_SET (signal_handler_asyncs, signum, async);
    }

  SCM_SIMPLE_VECTOR_SET (signal_handler_threads, signum, thread);
}

SCM
scm_sigaction (SCM signum, SCM handler, SCM flags)
{
  return scm_sigaction_for_thread (signum, handler, flags, SCM_UNDEFINED);
}

/* user interface for installation of signal handlers.  */
SCM_DEFINE (scm_sigaction_for_thread, "sigaction", 1, 3, 0,
           (SCM signum, SCM handler, SCM flags, SCM thread),
	    "Install or report the signal handler for a specified signal.\n\n"
	    "@var{signum} is the signal number, which can be specified using the value\n"
	    "of variables such as @code{SIGINT}.\n\n"
	    "If @var{handler} is omitted, @code{sigaction} returns a pair: the\n"
	    "CAR is the current\n"
	    "signal handler, which will be either an integer with the value @code{SIG_DFL}\n"
	    "(default action) or @code{SIG_IGN} (ignore), or the Scheme procedure which\n"
	    "handles the signal, or @code{#f} if a non-Scheme procedure handles the\n"
	    "signal.  The CDR contains the current @code{sigaction} flags for the handler.\n\n"
	    "If @var{handler} is provided, it is installed as the new handler for\n"
	    "@var{signum}.  @var{handler} can be a Scheme procedure taking one\n"
	    "argument, or the value of @code{SIG_DFL} (default action) or\n"
	    "@code{SIG_IGN} (ignore), or @code{#f} to restore whatever signal handler\n"
	    "was installed before @code{sigaction} was first used.  When\n"
	    "a scheme procedure has been specified, that procedure will run\n"
	    "in the given @var{thread}.   When no thread has been given, the\n"
	    "thread that made this call to @code{sigaction} is used.\n"
	    "Flags can optionally be specified for the new handler.\n"
	    "The return value is a pair with information about the\n"
	    "old handler as described above.\n\n"
	    "This interface does not provide access to the \"signal blocking\"\n"
	    "facility.  Maybe this is not needed, since the thread support may\n"
	    "provide solutions to the problem of consistent access to data\n"
	    "structures.")
#define FUNC_NAME s_scm_sigaction_for_thread
{
  int csig;
#ifdef HAVE_SIGACTION
  struct sigaction action;
  struct sigaction old_action;
#else
  void (* chandler) (int) = SIG_DFL;
  void (* old_chandler) (int);
#endif
  int query_only = 0;
  int save_handler = 0;
      
  SCM old_handler;

  csig = scm_to_signed_integer (signum, 0, NSIG-1);

#if defined(HAVE_SIGACTION)
  action.sa_flags = 0;
  if (!SCM_UNBNDP (flags))
    action.sa_flags |= scm_to_int (flags);
  sigemptyset (&action.sa_mask);
#endif

  if (SCM_UNBNDP (thread))
    thread = scm_current_thread ();
  else
    SCM_VALIDATE_THREAD (4, thread);

  scm_i_ensure_signal_delivery_thread ();

  scm_dynwind_begin (0);

  /* Among the pending asyncs, there might be signal handlers that will
     call this very function.  Thus, to avoid deadlocks, block asyncs
     before grabbing SIGNAL_HANDLER_LOCK.  */
  scm_dynwind_block_asyncs ();
  scm_i_dynwind_pthread_mutex_lock (&signal_handler_lock);

  old_handler = SCM_SIMPLE_VECTOR_REF (*signal_handlers, csig);
  if (SCM_UNBNDP (handler))
    query_only = 1;
  else if (scm_is_integer (handler))
    {
      intptr_t handler_int = scm_to_intptr_t (handler);

      if (handler_int == (intptr_t) SIG_DFL || handler_int == (intptr_t) SIG_IGN)
	{
#ifdef HAVE_SIGACTION
	  action.sa_handler = (void (*) (int)) handler_int;
#else
	  chandler = (void (*) (int)) handler_int;
#endif
	  install_handler (csig, SCM_BOOL_F, SCM_BOOL_F);
	}
      else
	{
	  SCM_OUT_OF_RANGE (2, handler);
	}
    }
  else if (scm_is_false (handler))
    {
      /* restore the default handler.  */
#ifdef HAVE_SIGACTION
      if (orig_handlers[csig].sa_handler == SIG_ERR)
	query_only = 1;
      else
	{
	  action = orig_handlers[csig];
	  orig_handlers[csig].sa_handler = SIG_ERR;
	  install_handler (csig, SCM_BOOL_F, SCM_BOOL_F);
	}
#else
      if (orig_handlers[csig] == SIG_ERR)
	query_only = 1;
      else
	{
	  chandler = orig_handlers[csig];
	  orig_handlers[csig] = SIG_ERR;
	  install_handler (csig, SCM_BOOL_F, SCM_BOOL_F);
	}
#endif
    }
  else
    {
      SCM_VALIDATE_PROC (2, handler);
#ifdef HAVE_SIGACTION
      action.sa_handler = take_signal;
      if (orig_handlers[csig].sa_handler == SIG_ERR)
	save_handler = 1;
#else
      chandler = take_signal;
      if (orig_handlers[csig] == SIG_ERR)
	save_handler = 1;
#endif
      install_handler (csig, thread, handler);
    }

  /* XXX - Silently ignore setting handlers for `program error signals'
     because they can't currently be handled by Scheme code.
  */

  switch (csig)
    {
      /* This list of program error signals is from the GNU Libc
         Reference Manual */
    case SIGFPE:
    case SIGILL:
    case SIGSEGV:
#ifdef SIGBUS
    case SIGBUS:
#endif
    case SIGABRT:
#if defined(SIGIOT) && (SIGIOT != SIGABRT)
    case SIGIOT:
#endif
#ifdef SIGTRAP
    case SIGTRAP:
#endif
#ifdef SIGEMT
    case SIGEMT:
#endif
#ifdef SIGSYS
    case SIGSYS:
#endif
      query_only = 1;
    }

#ifdef HAVE_SIGACTION
  if (query_only)
    {
      if (sigaction (csig, 0, &old_action) == -1)
	SCM_SYSERROR;
    }
  else
    {
      if (sigaction (csig, &action , &old_action) == -1)
	SCM_SYSERROR;
      if (save_handler)
	orig_handlers[csig] = old_action;
    }
  if (old_action.sa_handler == SIG_DFL || old_action.sa_handler == SIG_IGN)
    old_handler = scm_from_intptr_t ((intptr_t) old_action.sa_handler);

  scm_dynwind_end ();

  return scm_cons (old_handler, scm_from_int (old_action.sa_flags));
#else
  if (query_only)
    {
      if ((old_chandler = signal (csig, SIG_IGN)) == SIG_ERR)
	SCM_SYSERROR;
      if (signal (csig, old_chandler) == SIG_ERR)
	SCM_SYSERROR;
    }
  else
    {
      if ((old_chandler = signal (csig, chandler)) == SIG_ERR)
	SCM_SYSERROR;
      if (save_handler)
	orig_handlers[csig] = old_chandler;
    }
  if (old_chandler == SIG_DFL || old_chandler == SIG_IGN)
    old_handler = scm_from_intptr_t ((intptr_t) old_chandler);

  scm_dynwind_end ();

  return scm_cons (old_handler, scm_from_int (0));
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_restore_signals, "restore-signals", 0, 0, 0,
            (void),
	    "Return all signal handlers to the values they had before any call to\n"
	    "@code{sigaction} was made.  The return value is unspecified.")
#define FUNC_NAME s_scm_restore_signals
{
  int i;
  for (i = 0; i < NSIG; i++)
    {
#ifdef HAVE_SIGACTION
      if (orig_handlers[i].sa_handler != SIG_ERR)
	{
	  if (sigaction (i, &orig_handlers[i], NULL) == -1)
	    SCM_SYSERROR;
	  orig_handlers[i].sa_handler = SIG_ERR;
	  SCM_SIMPLE_VECTOR_SET (*signal_handlers, i, SCM_BOOL_F);
	}
#else
      if (orig_handlers[i] != SIG_ERR)
	{
	  if (signal (i, orig_handlers[i]) == SIG_ERR)
	    SCM_SYSERROR;
	  orig_handlers[i] = SIG_ERR;
	  SCM_SIMPLE_VECTOR_SET (*signal_handlers, i, SCM_BOOL_F);	  
	}
#endif
    }
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

#if HAVE_DECL_ALARM
SCM_DEFINE (scm_alarm, "alarm", 1, 0, 0,
           (SCM i),
	    "Set a timer to raise a @code{SIGALRM} signal after the specified\n"
	    "number of seconds (an integer).  It's advisable to install a signal\n"
	    "handler for\n"
	    "@code{SIGALRM} beforehand, since the default action is to terminate\n"
	    "the process.\n\n"
	    "The return value indicates the time remaining for the previous alarm,\n"
	    "if any.  The new value replaces the previous alarm.  If there was\n"
	    "no previous alarm, the return value is zero.")
#define FUNC_NAME s_scm_alarm
{
  return scm_from_uint (alarm (scm_to_uint (i)));
}
#undef FUNC_NAME
#endif /* HAVE_ALARM */

static void
pack_tv (struct timeval *tv, SCM seconds, SCM microseconds)
{
  tv->tv_sec = scm_to_long (seconds);
  tv->tv_usec = scm_to_long (microseconds);

  /* Allow usec to be outside the range [0, 999999).  */
  tv->tv_sec += tv->tv_usec / (1000 * 1000);
  tv->tv_usec %= 1000 * 1000;
}

static SCM
unpack_tv (const struct timeval *tv)
{
  return scm_cons (scm_from_long (tv->tv_sec), scm_from_long (tv->tv_usec));
}

#ifdef HAVE_SETITIMER
SCM_DEFINE (scm_setitimer, "setitimer", 5, 0, 0,
           (SCM which_timer,
            SCM interval_seconds, SCM interval_microseconds,
            SCM value_seconds, SCM value_microseconds),
            "Set the timer specified by @var{which_timer} according to the given\n"
            "@var{interval_seconds}, @var{interval_microseconds},\n"
            "@var{value_seconds}, and @var{value_microseconds} values.\n"
            "\n"
            "Return information about the timer's previous setting."
            "\n"
            "Errors are handled as described in the guile info pages under ``POSIX\n"
            "Interface Conventions''.\n"
            "\n"
            "The timers available are: @code{ITIMER_REAL}, @code{ITIMER_VIRTUAL},\n"
            "and @code{ITIMER_PROF}.\n"
            "\n"
            "The return value will be a list of two cons pairs representing the\n"
            "current state of the given timer.  The first pair is the seconds and\n"
            "microseconds of the timer @code{it_interval}, and the second pair is\n"
            "the seconds and microseconds of the timer @code{it_value}."
	    "\n"
	    "@code{ITIMER_PROF} or @code{ITIMER_VIRTUAL} are not supported on\n"
	    "some platforms and will always error. @code{(provided? 'ITIMER_PROF)}\n"
	    "and @code{(provided? 'ITIMER_VIRTUAL)} report whether those timers\n"
	    "are supported.\n")

#define FUNC_NAME s_scm_setitimer
{
  int rv;
  int c_which_timer;
  struct itimerval new_timer;
  struct itimerval old_timer;

  c_which_timer = SCM_NUM2INT(1, which_timer);
  pack_tv (&new_timer.it_interval, interval_seconds, interval_microseconds);
  pack_tv (&new_timer.it_value, value_seconds, value_microseconds);

  SCM_SYSCALL(rv = setitimer(c_which_timer, &new_timer, &old_timer));
  
  if(rv != 0)
    SCM_SYSERROR;

  return scm_list_2 (unpack_tv (&old_timer.it_interval),
                     unpack_tv (&old_timer.it_value));
}
#undef FUNC_NAME
#endif /* HAVE_SETITIMER */

#ifdef HAVE_GETITIMER
SCM_DEFINE (scm_getitimer, "getitimer", 1, 0, 0,
  (SCM which_timer),
            "Return information about the timer specified by @var{which_timer}"
            "\n"
            "Errors are handled as described in the guile info pages under ``POSIX\n"
            "Interface Conventions''.\n"
            "\n"
            "The timers available are: @code{ITIMER_REAL}, @code{ITIMER_VIRTUAL},\n"
            "and @code{ITIMER_PROF}.\n"
            "\n"
            "The return value will be a list of two cons pairs representing the\n"
            "current state of the given timer.  The first pair is the seconds and\n"
            "microseconds of the timer @code{it_interval}, and the second pair is\n"
            "the seconds and microseconds of the timer @code{it_value}."
	    "\n"
	    "@code{ITIMER_PROF} or @code{ITIMER_VIRTUAL} are not supported on\n"
	    "some platforms and will always error. @code{(provided? 'ITIMER_PROF)}\n"
	    "and @code{(provided? 'ITIMER_VIRTUAL)} report whether those timers\n"
	    "are supported.\n")
#define FUNC_NAME s_scm_getitimer
{
  int rv;
  int c_which_timer;
  struct itimerval old_timer;

  c_which_timer = SCM_NUM2INT(1, which_timer);

  SCM_SYSCALL(rv = getitimer(c_which_timer, &old_timer));

  if(rv != 0)
    SCM_SYSERROR;

  return scm_list_2 (scm_cons (scm_from_long (old_timer.it_interval.tv_sec),
			       scm_from_long (old_timer.it_interval.tv_usec)),
		     scm_cons (scm_from_long (old_timer.it_value.tv_sec),
			       scm_from_long (old_timer.it_value.tv_usec)));
}
#undef FUNC_NAME
#endif /* HAVE_GETITIMER */

#ifdef HAVE_PAUSE
SCM_DEFINE (scm_pause, "pause", 0, 0, 0,
           (),
	    "Pause the current process (thread?) until a signal arrives whose\n"
	    "action is to either terminate the current process or invoke a\n"
	    "handler procedure.  The return value is unspecified.")
#define FUNC_NAME s_scm_pause
{
  pause ();
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME
#endif

SCM_DEFINE (scm_sleep, "sleep", 1, 0, 0,
           (SCM i),
	    "Wait for the given number of seconds (an integer) or until a signal\n"
	    "arrives.  The return value is zero if the time elapses or the number\n"
	    "of seconds remaining otherwise.\n"
	    "\n"
	    "See also @code{usleep}.")
#define FUNC_NAME s_scm_sleep
{
  return scm_from_uint (scm_std_sleep (scm_to_uint (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_usleep, "usleep", 1, 0, 0,
           (SCM i),
	    "Wait the given period @var{usecs} microseconds (an integer).\n"
	    "If a signal arrives the wait stops and the return value is the\n"
	    "time remaining, in microseconds.  If the period elapses with no\n"
	    "signal the return is zero.\n"
	    "\n"
	    "On most systems the process scheduler is not microsecond accurate and\n"
	    "the actual period slept by @code{usleep} may be rounded to a system\n"
	    "clock tick boundary.  Traditionally such ticks were 10 milliseconds\n"
	    "apart, and that interval is often still used.\n"
	    "\n"
	    "See also @code{sleep}.")
#define FUNC_NAME s_scm_usleep
{
  return scm_from_ulong (scm_std_usleep (scm_to_ulong (i)));
}
#undef FUNC_NAME

SCM_DEFINE (scm_raise, "raise", 1, 0, 0,
           (SCM sig),
	    "Sends a specified signal @var{sig} to the current process, where\n"
	    "@var{sig} is as described for the kill procedure.")
#define FUNC_NAME s_scm_raise
{
  if (raise (scm_to_int (sig)) != 0)
    SCM_SYSERROR;
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



void
scm_i_close_signal_pipe()
{
  /* SIGNAL_DELIVERY_THREAD_MUTEX is only locked while the signal delivery
     thread is being launched.  The thread that calls this function is
     already holding the thread admin mutex, so if the delivery thread hasn't
     been launched at this point, it never will be before shutdown.  */
  scm_i_pthread_mutex_lock (&signal_delivery_thread_mutex);

#if SCM_USE_PTHREAD_THREADS
  if (scm_i_signal_delivery_thread != NULL)
    close (signal_pipe[1]);
#endif

  scm_i_pthread_mutex_unlock (&signal_delivery_thread_mutex);
}

void
scm_init_scmsigs ()
{
  int i;

  signal_handlers =
    SCM_VARIABLE_LOC (scm_c_define ("signal-handlers",
				  scm_c_make_vector (NSIG, SCM_BOOL_F)));
  signal_handler_asyncs = scm_c_make_vector (NSIG, SCM_BOOL_F);
  signal_handler_threads = scm_c_make_vector (NSIG, SCM_BOOL_F);

  for (i = 0; i < NSIG; i++)
    {
#ifdef HAVE_SIGACTION
      orig_handlers[i].sa_handler = SIG_ERR;

#else
      orig_handlers[i] = SIG_ERR;
#endif
    }

  scm_c_define ("NSIG", scm_from_long (NSIG));
  scm_c_define ("SIG_IGN", scm_from_intptr_t ((intptr_t) SIG_IGN));
  scm_c_define ("SIG_DFL", scm_from_intptr_t ((intptr_t) SIG_DFL));
#ifdef SA_NOCLDSTOP
  scm_c_define ("SA_NOCLDSTOP", scm_from_long (SA_NOCLDSTOP));
#endif
#ifdef SA_RESTART
  scm_c_define ("SA_RESTART", scm_from_long (SA_RESTART));
#endif

#if defined(HAVE_SETITIMER) || defined(HAVE_GETITIMER)
  /* Stuff needed by setitimer and getitimer. */
  scm_c_define ("ITIMER_REAL", scm_from_int (ITIMER_REAL));
  scm_c_define ("ITIMER_VIRTUAL", scm_from_int (ITIMER_VIRTUAL));
  scm_c_define ("ITIMER_PROF", scm_from_int (ITIMER_PROF));
#ifdef HAVE_USABLE_GETITIMER_PROF
  scm_add_feature ("ITIMER_PROF");
#endif
#ifdef HAVE_USABLE_GETITIMER_VIRTUAL
  scm_add_feature ("ITIMER_VIRTUAL");
#endif
#endif /* defined(HAVE_SETITIMER) || defined(HAVE_GETITIMER) */

#include "scmsigs.x"
}

