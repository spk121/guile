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

static char *
string_trim_both (char *buf)
{
  char *start, *end;
  char *str;

  start = buf;
  end = buf + strlen(buf);
  while (start < end && (start[0] == ' ' || start[0] == '\t'))
    start ++;
  while (end > start
         && (end[-1] == ' ' || end[-1] == '\t'
             || end[-1] == '\r' || end[-1] == '\n'))
    end --;
  if (start == end)
    return NULL;
  str = (char *) calloc (end - start + 1, sizeof (char));
  if (str == NULL)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }
  memcpy (str, start, end - start);
  str[end - start + 1] = '\0';
  return str;
}

static char **
read_command_file (char *argv0, int *n)
{
  int i = 0;
  int n_max = 5;
  char **new_argv;
  char buf[1024];
  char *str;

  FILE *fp = fopen("cmdargs.txt", "rt");
  if (fp == NULL)
    {
      *n = 1;
      return NULL;
    }

  new_argv = (char **) malloc (n_max * sizeof (char *));
  if (new_argv == NULL || (new_argv[i] = strdup (argv0)) == NULL)
    {
      fprintf (stderr, "Out of memory\n");
      exit (1);
    }

  i ++;
  while (1)
    {
      if (fgets (buf, 1024, fp) == NULL)
        break;

      if (buf[0] == '#')
        continue;
      str = string_trim_both (buf);
      if (str != NULL)
        {
          new_argv[i] = str;
          i ++;

          if (i >= n_max)
            {
              n_max *= 2;
              new_argv = (char **) realloc (new_argv, n_max * sizeof (char *));
            }
        }
    }
  *n = i;
  new_argv[i] = NULL;
  return new_argv;
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

  if (argc == 1)
    {
      char **new_argv;
      int n;
      new_argv = read_command_file (argv[0], &n);
      if (new_argv != NULL)
        scm_boot_guile (n, new_argv, inner_main, 0);
    }

  scm_boot_guile (argc, argv, inner_main, 0);
  return 0; /* never reached */
}
