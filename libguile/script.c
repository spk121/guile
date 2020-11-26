/* Copyright 1994-1998,2000-2011,2013-2014,2018,2020
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

/* "script.c" argv tricks for `#!' scripts.
   Authors: Aubrey Jaffer and Jim Blandy */

#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <ctype.h>
#include <errno.h>
#include <localcharset.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <uniconv.h>
#include <unistd.h>		/* for X_OK define */

#ifdef HAVE_IO_H
#include <io.h>
#endif

#include "eval.h"
#include "feature.h"
#include "fluids.h"
#include "load.h"
#include "modules.h"
#include "pairs.h"
#include "read.h"
#include "strings.h"
#include "strports.h"
#include "throw.h"
#include "version.h"
#include "vm.h"

#include "script.h"


#ifndef WHITE_SPACES
#ifdef MSDOS
#define WHITE_SPACES  ' ':case '\t':case '\r':case '\f':case 26
#else
#define WHITE_SPACES  ' ':case '\t':case '\r':case '\f'
#endif /* def MSDOS */
#endif /* ndef LINE_INCREMENTORS */


/* Read a \nnn-style escape.  We've just read the backslash.  */
static int
script_get_octal (FILE *f)
#define FUNC_NAME "script_get_octal"
{
  int i;
  int value = 0;

  for (i = 0; i < 3; i++)
    {
      int c = getc (f);
      if ('0' <= c && c <= '7')
	value = (value * 8) + (c - '0');
      else
	SCM_MISC_ERROR ("malformed script: bad octal backslash escape",
			SCM_EOL);
    }
  return value;
}
#undef FUNC_NAME


static int
script_get_backslash (FILE *f)
#define FUNC_NAME "script_get_backslash"
{
  int c = getc (f);

  switch (c)
    {
    case 'a': return '\a';
    case 'b': return '\b';
    case 'f': return '\f';
    case 'n': return '\n';
    case 'r': return '\r';
    case 't': return '\t';
    case 'v': return '\v';

    case '\\':
    case ' ':
    case '\t':
    case '\n':
      return c;

    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
      ungetc (c, f);
      return script_get_octal (f);

    case EOF:
      SCM_MISC_ERROR ("malformed script: backslash followed by EOF", SCM_EOL);
      return 0; /* not reached? */

    default:
      SCM_MISC_ERROR ("malformed script: bad backslash sequence", SCM_EOL);
      return 0; /* not reached? */
    }
}
#undef FUNC_NAME

/*
 * Like `realloc', but free memory on failure;
 * unlike `scm_realloc', return NULL, not aborts.
*/
static void*
realloc0 (void *ptr, size_t size)
{
  void *new_ptr = realloc (ptr, size);
  if (!new_ptr)
    {
      free (ptr);
    }
  return new_ptr;
}


static char *
script_read_arg (FILE *f)
#define FUNC_NAME "script_read_arg"
{
  size_t size = 7;
  char *buf = scm_malloc (size + 1);
  size_t len = 0;

  if (! buf)
    return 0;

  for (;;)
    {
      int c = getc (f);
      switch (c)
	{
	case '\\':
	  c = script_get_backslash (f);
	  /* The above produces a new character to add to the argument.
             Fall through.  */
	default:
	  if (len >= size)
	    {
	      size = (size + 1) * 2;
	      buf = realloc0 (buf, size);
	      if (! buf)
		return 0;
	    }
	  buf[len++] = c;
	  break;

	case '\n':
	  /* This may terminate an arg now, but it will terminate the
             entire list next time through.  */
	  ungetc ('\n', f);
	case EOF:
	  if (len == 0)
	    {
	      free (buf);
	      return 0;
	    }
	  /* Otherwise, those characters terminate the argument; fall
             through.  */
	case ' ':
	  buf[len] = '\0';
	  return buf;

	case '\t':
	  free (buf);
	  SCM_MISC_ERROR ("malformed script: TAB in meta-arguments", SCM_EOL);
	  return 0; /* not reached? */
	}
    }
}
#undef FUNC_NAME


static int
script_meta_arg_P (char *arg)
{
  if ('\\' != arg[0])
    return 0L;
#ifdef MSDOS
  return !arg[1];
#else
  switch (arg[1])
    {
    case 0:
    case '%':
    case WHITE_SPACES:
      return !0;
    default:
      return 0L;
    }
#endif
}

char **
scm_get_meta_args (int argc, char **argv)
{
  int nargc = argc, argi = 1, nargi = 1;
  char *narg, **nargv;
  if (!(argc > 2 && script_meta_arg_P (argv[1])))
    return 0L;
  if (!(nargv = (char **) scm_malloc ((1 + nargc) * sizeof (char *))))
      return 0L;
  nargv[0] = argv[0];
  while (((argi + 1) < argc) && (script_meta_arg_P (argv[argi])))
    {
      FILE *f = fopen (argv[++argi], "r");
      if (f)
	{
	  nargc--;		/* to compensate for replacement of '\\' */
	  while (1)
	    switch (getc (f))
	      {
	      case EOF:
                free (nargv);
		return 0L;
	      default:
		continue;
	      case '\n':
		goto found_args;
	      }
	found_args:
          /* FIXME: we leak the result of calling script_read_arg.  */
	  while ((narg = script_read_arg (f)))
	    if (!(nargv = (char **) realloc0 (nargv,
					     (1 + ++nargc) * sizeof (char *))))
	      return 0L;
	    else
	      nargv[nargi++] = narg;
	  fclose (f);
	  nargv[nargi++] = argv[argi++];
	}
    }
  while (argi <= argc)
    nargv[nargi++] = argv[argi++];
  return nargv;
}

int
scm_count_argv (char **argv)
{
  int argc = 0;
  while (argv[argc])
    argc++;
  return argc;
}


/* For use in error messages.  */
char *scm_usage_name = 0;

void
scm_shell_usage (int fatal, char *message)
{
  scm_call_3 (scm_c_private_ref ("ice-9 command-line",
                                 "shell-usage"),
              (scm_usage_name
               ? scm_from_locale_string (scm_usage_name)
               : scm_from_latin1_string ("guile")),
              scm_from_bool (fatal),
              (message
               ? scm_from_locale_string (message)
               : SCM_BOOL_F));
}

static SCM
locale_arguments_to_string_list (int argc, char **const argv)
{
  int i;
  SCM lst;

  for (i = argc - 1, lst = SCM_EOL;
       i >= 0;
       i--)
    lst = scm_cons (scm_from_locale_stringn (argv[i], (size_t) -1), lst);

  return lst;
}

/* Set the value returned by `program-arguments', given ARGC and ARGV.  */
void
scm_i_set_boot_program_arguments (int argc, char *argv[])
{
  scm_fluid_set_x (scm_program_arguments_fluid,
		   locale_arguments_to_string_list (argc, argv));
}

/* Given an array of command-line switches, return a Scheme expression
   to carry out the actions specified by the switches.
 */

SCM
scm_compile_shell_switches (int argc, char **argv)
{
  return scm_call_2 (scm_c_public_ref ("ice-9 command-line",
                                       "compile-shell-switches"),
		     locale_arguments_to_string_list (argc, argv),
                     (scm_usage_name
                      ? scm_from_locale_string (scm_usage_name)
                      : scm_from_latin1_string ("guile")));
}


void
scm_shell (int argc, char **argv)
{
  /* If present, add SCSH-style meta-arguments from the top of the
     script file to the argument vector.  See the SCSH manual: "The
     meta argument" for more details.  */
  {
    char **new_argv = scm_get_meta_args (argc, argv);

    if (new_argv)
      {
	argv = new_argv;
	argc = scm_count_argv (new_argv);
      }
  }

  exit (scm_exit_status (scm_eval_x (scm_compile_shell_switches (argc, argv),
				     scm_current_module ())));
}


void
scm_init_script ()
{
#include "script.x"
}
