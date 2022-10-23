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
#  include <config.h>
#endif

#ifdef __MINGW32__

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include "bytevectors.h"
#include "gc.h"
#include "strings.h"
#include "symbols.h"
#include "w32conport.h"

/* This is a MinGW specific port for terminal I/O.  The con port type
   tries to simplify the stdin/stdout/stderr situation by choosing a
   subset of Windows 11 Console subsystem functionality that is closer
   to the expectations of Guile.

   - The input is cooked, blocking, and line delimited and is received
     in UTF-8.

   - The output is UTF-8 with ECMA-48 control characters that get passed
     to the Windows Console.

   The input has to be blocking. Guile only handles non-blocking reads
   from ports for which an integer file descriptor can be used in a poll
   function.  Windows has integer file descriptors, but, UCRT does not
   have poll.
*/

static scm_t_port_type *scm_conport_type;

/* struct allocated for each buffered CONPORT.  */
typedef struct _scm_t_conport {
  /* One of STD_INPUT_HANDLE, STD_OUTPUT_HANDLE, STD_ERROR_HANDLE.  */
  DWORD device;
  /* The handle to a specified standard device, or INVALID_HANDLE_VALUE  */
  HANDLE handle;
  /* Flags for GetConsoleMode SetConsoleMode*/
  DWORD orig_mode;
  DWORD mode;
} scm_t_conport;
  
/* Fill a port's read-buffer with a single read, which should normally
   be a line-buffered text. */
static size_t
conport_read (SCM port, SCM dst, size_t start, size_t count)
{
  scm_t_conport *cp = (scm_t_conport *) SCM_STREAM (port);
  signed char *ptr = SCM_BYTEVECTOR_CONTENTS (dst) + start;
  size_t n;
  n = fread(ptr, 1, count, stdin);
  return n;
}

static size_t
conport_write (SCM port, SCM src, size_t start, size_t count)
{
  scm_t_conport *cp = (scm_t_conport *) SCM_STREAM (port);
  signed char *ptr = SCM_BYTEVECTOR_CONTENTS (src) + start;
  size_t n;
  if (cp->device == STD_ERROR_HANDLE)
    n = fwrite(ptr, 1, count, stderr);
  else
    n = fwrite(ptr, 1, count, stdout);
  return n;
}

static int
conport_input_waiting (SCM port)
{
  return 0;
}

static scm_t_port_type *
scm_make_conptob ()
{
  scm_t_port_type *ptob = scm_make_port_type ("console", conport_read, conport_write);
  scm_set_port_input_waiting (ptob, conport_input_waiting);
  return ptob;
}

SCM
scm_make_conport(int fd)
{
  SCM port = SCM_BOOL_F;
  SCM encoding = scm_from_latin1_string ("UTF-8");
  SCM strategy = scm_from_latin1_symbol ("escape");
  scm_t_conport *cp;

  cp = (scm_t_conport *) scm_gc_malloc_pointerless (sizeof (scm_t_conport),
                                                    "console port");
  memset (cp, 0, sizeof (scm_t_conport));

  if (fd == 0)
    cp->device = STD_INPUT_HANDLE;
  else if (fd == 1)
    cp->device = STD_OUTPUT_HANDLE;
  else if (fd == 2)
    cp->device = STD_ERROR_HANDLE;
  cp->handle = GetStdHandle (cp->device);
  cp->orig_mode = 0;
  cp->mode = 0;
  GetConsoleMode (cp->handle, &cp->orig_mode);

  /* Tell Windows console that we're using ECMA-48 style excape codes. */
  if (fd == 0)
    SetConsoleMode (cp->handle, cp->orig_mode | ENABLE_VIRTUAL_TERMINAL_INPUT | ENABLE_LINE_INPUT);
  else if ((fd == 1) || (fd == 2))
    SetConsoleMode (cp->handle, cp->orig_mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
  GetConsoleMode (cp->handle, &cp->mode);
  
  /* Set I/O to UTF-8. */
  SetConsoleCP (65001);
  SetConsoleOutputCP (65001);
  
  // We use BUF0 (unbuffered) because input from the console itself
  // is already line buffered.
  if (fd == 0)
    //port = scm_c_make_port_with_encoding(scm_conport_type, SCM_RDNG | SCM_BUF0,
    //                                     encoding, strategy, (scm_t_bits) cp);
    port = scm_c_make_port(scm_conport_type, SCM_RDNG | SCM_BUF0, (scm_t_bits) cp);
  else if (fd == 1 || fd == 2)
    //port = scm_c_make_port_with_encoding(scm_conport_type, SCM_WRTNG | SCM_BUF0,
    //                                     encoding, strategy, (scm_t_bits) cp);
    port = scm_c_make_port(scm_conport_type, SCM_WRTNG | SCM_BUF0, (scm_t_bits) cp);
  return port;
}

void
scm_init_conports ()
{
  scm_conport_type = scm_make_conptob ();
}

#else /* !__MINGW32__ */

void
scm_init_conports ()
{

}

#endif /* !__MINGW32__ */
