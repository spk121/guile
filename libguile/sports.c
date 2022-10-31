/* Copyright 2022
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
#include <config.h>
#endif

#if defined(_WIN32) && !defined(__CYGWIN__)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif
#include <stdio.h>
#include <string.h>
#include "boolean.h"
#include "bytevectors.h"
#include "gc.h"
#include "gsubr.h"
#include "list.h"
#include "strings.h"
#include "symbols.h"
#include "sports.h"

/* This is a specific port for terminal I/O that can be used when fports
   are misbehaving due to a broken or limited support for poll() on the
   file descriptors 0, 1 and 2. The stream_port type uses FILE *
   stdin/stdout/stderr and avoids operating directly on the integer file
   descriptors.

   The input has to be blocking. Guile only handles non-blocking reads
   from ports for which an integer file descriptor can be used in a poll
   function.
*/

/* Struct allocated for each STREAM_PORT.  */
typedef struct _scm_t_stream_port
{
  /* The descriptor ID. It should only be 0, 1, or 2 for stdin, stdout,
     and stderr respectively. */
  int id;
} scm_t_stream_port;

//int scm_use_stream_ports = SCM_USE_STREAM_PORTS;
int scm_use_stream_ports = 1;
static scm_t_port_type *scm_stream_port_type;
static SCM sym_stdin;
static SCM sym_stdout;
static SCM sym_stderr;

/* Fill a port's read-buffer with a single read, which should normally
   be a line-buffered text. */
static size_t
stream_port_read (SCM port, SCM dst, size_t start, size_t count)
{
  scm_t_stream_port *ssp = (scm_t_stream_port *) SCM_STREAM (port);
  signed char *ptr = SCM_BYTEVECTOR_CONTENTS (dst) + start;

  if (ssp->id == 0)
    return fread (ptr, 1, count, stdin);
  return 0;
}

static size_t
stream_port_write (SCM port, SCM src, size_t start, size_t count)
{
  scm_t_stream_port *ssp = (scm_t_stream_port *) SCM_STREAM (port);
  signed char *ptr = SCM_BYTEVECTOR_CONTENTS (src) + start;

  if (ssp->id == 1)
    return fwrite (ptr, 1, count, stdout);
  else if (ssp->id == 2)
    return fwrite (ptr, 1, count, stderr);
  return 0;
}

/* With stdin, there is no portable method of determining if input is
   waiting. */
static int
stream_port_input_waiting (SCM port)
{
  return 0;
}

static scm_t_port_type *
scm_make_stream_ptob ()
{
  scm_t_port_type *ptob;
  ptob =
    scm_make_port_type ("stream port", stream_port_read,
			stream_port_write);
  scm_set_port_input_waiting (ptob, stream_port_input_waiting);
  return ptob;
}

SCM
scm_i_make_stream_port (int fd)
{
  SCM port = SCM_BOOL_F;
  scm_t_stream_port *ssp;

  ssp =
    (scm_t_stream_port *)
    scm_gc_malloc_pointerless (sizeof (scm_t_stream_port), "stream_port");
  memset (ssp, 0, sizeof (scm_t_stream_port));
  ssp->id = fd;
  if (fd == 0)
    port = scm_c_make_port (scm_stream_port_type, SCM_RDNG | SCM_BUF0,
			    (scm_t_bits) ssp);
  else if (fd == 1 || fd == 2)
    port = scm_c_make_port (scm_stream_port_type, SCM_WRTNG | SCM_BUF0,
			    (scm_t_bits) ssp);
  return port;
}

SCM_DEFINE (scm_make_stream_port, "make-stream-port", 1, 0, 0,
	    (SCM source),
	    "Creates a stream port, which is a simplistic fallback\n"
	    "port that can be used for standard input, output and error.\n"
	    "@code{source} is a symbol that is one of @code{stdin} @code{stdout}\n"
	    "or @code{stderr}.  Returns a new port.")
#define FUNC_NAME s_scm_make_stream_port
{
  SCM_ASSERT_TYPE (scm_is_true (scm_symbol_p (source)), source, SCM_ARG1,
		   FUNC_NAME, "symbol");

  int fd;
  if (scm_is_eq (source, sym_stdin))
    fd = 0;
  else if (scm_is_eq (source, sym_stdout))
    fd = 1;
  else if (scm_is_eq (source, sym_stderr))
    fd = 2;
  else
    scm_misc_error (FUNC_NAME, "unknown standard stream ~S",
		    scm_list_1 (source));

  return scm_i_make_stream_port (fd);
}

#undef FUNC_NAME

#if defined(_WIN32) && !defined(__CYGWIN__)
/* Set Windows Terminal to an xterm mode. */
static void
do_win32_stream_port_setup (void)
{
  DWORD device;
  HANDLE handle;
  DWORD orig_mode;

  /* Set I/O to UTF-8. */
  SetConsoleCP (65001);
  SetConsoleOutputCP (65001);

  for (int fd = 0; fd < 3; fd++)
    {
      if (fd == 0)
	device = STD_INPUT_HANDLE;
      else if (fd == 1)
	device = STD_OUTPUT_HANDLE;
      else if (fd == 2)
	device = STD_ERROR_HANDLE;
      handle = GetStdHandle (device);
      orig_mode = 0;
      GetConsoleMode (handle, &orig_mode);

      if (fd == 0)
	/* Use Windows Console's default line-editor. */
	SetConsoleMode (handle,
			orig_mode
			| ENABLE_LINE_INPUT
			| ENABLE_PROCESSED_INPUT
			| ENABLE_VIRTUAL_TERMINAL_INPUT);
      else if ((fd == 1) || (fd == 2))
	/* Tell Windows console that we're using ECMA-48 style escape
	   codes. */
	SetConsoleMode (handle,
			orig_mode
			| ENABLE_PROCESSED_OUTPUT
			| ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    }
}
#endif

SCM_DEFINE (scm_stream_port_p, "stream-port?", 1, 0, 0,
	    (SCM obj),
	    "Determine whether @var{obj} is a stream port.")
#define FUNC_NAME s_scm_file_port_p
{
  return scm_from_bool (SCM_PORTP (obj)
			&& SCM_PORT_TYPE (obj) == scm_stream_port_type);
}

#undef FUNC_NAME

void
scm_init_stream_ports ()
{
  char *env;
  scm_stream_port_type = scm_make_stream_ptob ();
  sym_stdin = scm_from_latin1_symbol ("stdin");
  sym_stdout = scm_from_latin1_symbol ("stdout");
  sym_stderr = scm_from_latin1_symbol ("stderr");

  env = getenv ("SCM_FALLBACK_PORTS");
  if (env && strcmp (env, "1") == 0)
    scm_use_stream_ports = 1;
  else if (env && strcmp (env, "0") == 0)
    scm_use_stream_ports = 0;
#if defined(_WIN32) && !defined(__CYGWIN__)
  if (scm_use_stream_ports)
    do_win32_stream_port_setup ();
#endif
#include "sports.x"
}
