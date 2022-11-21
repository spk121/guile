/* Copyright 1996-1997,2000-2001,2006,2008,2011,2013,2018,2021
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

/* This is the 'main' function for the `guile' executable.  It is not
   included in libguile.a.

   Eventually, we hope this file will be automatically generated,
   based on the list of installed, statically linked libraries on the
   system.  For now, please don't put interesting code in here.  */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <locale.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef HAVE_WINSOCK2_H
#include <winsock2.h>
#endif

#include <libguile.h>

// from load.c
extern char *scm_i_self_path;

#if defined(_WIN32) && !defined(__CYGWIN__)
#include <unistr.h>
/* used outside this file: */
static char *get_self_path(char *exec_file)
{
  wchar_t *path;
  char *u8path;
  DWORD r, sz = 1024;
  size_t len;

  while (1) {
    path = (wchar_t *)malloc(sz * sizeof(wchar_t));
    r = GetModuleFileNameW(NULL, path, sz);
    if ((r == sz)
        && (GetLastError() == ERROR_INSUFFICIENT_BUFFER)) {
      // free(path);
      sz = 2 * sz;
    } else
      break;
  }

  u8path = u16_to_u8 (path, wcslen(path), NULL, &len);

  // strip off the last element, which should be the .exe
  for (size_t i = len - 1; i >= 0; i --)
    if (u8path[i] == '\\')
      {
        u8path[i] = '\0';
        break;
      }

  return u8path;
}
#elif defined(__linux__)
# include <errno.h>
# include <unistd.h>
static char *get_self_path(char *exec_file)
{
  char *s;
  ssize_t len, blen = 256;

  s = malloc(blen);

  while (1) {
    len = readlink("/proc/self/exe", s, blen-1);
    if (len == (blen-1)) {
      free(s);
      blen *= 2;
      s = malloc(blen);
    } else if (len < 0) {
      fprintf(stderr, "failed to get self (%d)\n", errno);
      exit(1);
    } else
      break;
  }
  s[len] = '\0';
  for (size_t i = len - 1; i >= 0; i --)
    if (s[i] == '/')
      {
        s[i] = '\0';
        break;
      }

  return s;
}
#endif

static void
inner_main (void *closure SCM_UNUSED, int argc, char **argv)
{
#ifdef __MINGW32__
  /* This is necessary to startup the Winsock API under Win32. */
  WSADATA WSAData;
  WSAStartup (0x0202, &WSAData);
#endif /* __MINGW32__ */

  /* module initializations would go here */
  scm_shell (argc, argv);

#ifdef __MINGW32__
  WSACleanup ();
#endif /* __MINGW32__ */
}

static int
get_integer_from_environment (const char *var, int def)
{
  char *end = 0;
  char *val = getenv (var);
  long res = def;
  if (!val)
    return def;
  res = strtol (val, &end, 10);
  if (end == val)
    {
      fprintf (stderr, "guile: warning: invalid %s: %s\n", var, val);
      return def;
    }
  return res;
}

static int
should_install_locale (void)
{
  /* If the GUILE_INSTALL_LOCALE environment variable is unset,
     or set to a nonzero value, we should install the locale via
     setlocale().  */
  return get_integer_from_environment ("GUILE_INSTALL_LOCALE", 1);
}

int
main (int argc, char **argv)
{
  /* If we should install a locale, do it right at the beginning so that
     string conversion for command-line arguments, along with possible
     error messages, use the right locale.  See
     <https://lists.gnu.org/archive/html/guile-devel/2011-11/msg00041.html>
     for the rationale.  */
  if (should_install_locale () && setlocale (LC_ALL, "") == NULL)
    fprintf (stderr, "guile: warning: failed to install locale\n");

  scm_i_self_path = get_self_path (argv[0]);
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
