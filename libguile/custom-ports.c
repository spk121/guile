/* Copyright 2023
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

#include "boolean.h"
#include "eval.h"
#include "extensions.h"
#include "gsubr.h"
#include "modules.h"
#include "numbers.h"
#include "ports-internal.h"
#include "syscalls.h"
#include "values.h"
#include "variable.h"
#include "version.h"

#include "custom-ports.h"


#define FOR_EACH_METHOD_EXCEPT_READ_WRITE(M) \
  M(print, "print") \
  M(read_wait_fd, "read-wait-fd") \
  M(write_wait_fd, "write-wait-fd") \
  M(seek, "seek") \
  M(close, "close") \
  M(get_natural_buffer_sizes, "get-natural-buffer-sizes") \
  M(random_access_p, "random-access?") \
  M(input_waiting, "input-waiting?") \
  M(truncate, "truncate")

#define FOR_EACH_METHOD(M)             \
  FOR_EACH_METHOD_EXCEPT_READ_WRITE(M) \
  M(read, "read") \
  M(write, "write")

#define DEF_VAR(c_name, scm_name) static SCM c_name##_var;
FOR_EACH_METHOD (DEF_VAR)
#undef DEF_VAR
     static int custom_port_print (SCM exp, SCM port,
                                   scm_print_state * pstate)
{
  SCM data = SCM_PACK (SCM_STREAM (exp));
  scm_call_3 (scm_variable_ref (print_var), exp, data, port);
  return 1;
}

static int
custom_port_read_wait_fd (SCM port)
{
  SCM data = SCM_PACK (SCM_STREAM (port));
  SCM res = scm_call_2 (scm_variable_ref (read_wait_fd_var), port, data);
  return scm_is_false (res) ? -1 : scm_to_signed_integer (res, 0, INT_MAX);
}

static int
custom_port_write_wait_fd (SCM port)
{
  SCM data = SCM_PACK (SCM_STREAM (port));
  SCM res = scm_call_2 (scm_variable_ref (write_wait_fd_var), port, data);
  return scm_is_false (res) ? -1 : scm_to_signed_integer (res, 0, INT_MAX);
}

static scm_t_off
custom_port_seek (SCM port, scm_t_off offset, int whence)
{
  SCM data = SCM_PACK (SCM_STREAM (port));
  return scm_to_off_t (scm_call_4 (scm_variable_ref (seek_var), port, data,
                                   scm_from_off_t (offset),
                                   scm_from_int (whence)));
}

static void
custom_port_close (SCM port)
{
  SCM data = SCM_PACK (SCM_STREAM (port));
  scm_call_2 (scm_variable_ref (close_var), port, data);
}

static void
custom_port_get_natural_buffer_sizes (SCM port, size_t *read_size,
                                      size_t *write_size)
{
  SCM data = SCM_PACK (SCM_STREAM (port));
  SCM res = scm_call_4 (scm_variable_ref (get_natural_buffer_sizes_var),
                        port, data, scm_from_size_t (*read_size),
                        scm_from_size_t (*write_size));
  *read_size = scm_to_size_t (scm_c_value_ref (res, 0));
  *write_size = scm_to_size_t (scm_c_value_ref (res, 1));
}

static int
custom_port_random_access_p (SCM port)
{
  SCM data = SCM_PACK (SCM_STREAM (port));
  return scm_to_bool (scm_call_2 (scm_variable_ref (random_access_p_var),
                                  port, data));
}

static int
custom_port_input_waiting (SCM port)
{
  SCM data = SCM_PACK (SCM_STREAM (port));
  return scm_to_bool (scm_call_2 (scm_variable_ref (input_waiting_var),
                                  port, data));
}

static void
custom_port_truncate (SCM port, scm_t_off length)
{
  SCM data = SCM_PACK (SCM_STREAM (port));
  scm_call_3 (scm_variable_ref (truncate_var), port, data,
              scm_from_off_t (length));
}

static scm_t_port_type *custom_port_type;
static scm_t_port_type *custom_port_type_with_close_on_gc;

SCM_DEFINE_STATIC (make_custom_port, "%make-custom-port", 6, 0, 0,
                   (SCM input_p, SCM output_p, SCM stream, SCM encoding,
                    SCM conversion_strategy, SCM close_on_gc_p), "")
{
  long mode_bits = 0;
  if (scm_is_true (input_p))
    mode_bits |= SCM_RDNG;
  if (scm_is_true (output_p))
    mode_bits |= SCM_WRTNG;

  scm_t_port_type *pt = scm_is_true (close_on_gc_p) ?
    custom_port_type_with_close_on_gc : custom_port_type;

  return scm_c_make_port_with_encoding (pt, mode_bits, encoding,
                                        conversion_strategy,
                                        SCM_UNPACK (stream));
}

SCM_DEFINE_STATIC (custom_port_data, "%custom-port-data", 1, 0, 0,
                   (SCM port), "")
#define FUNC_NAME s_custom_port_data
{
  SCM_ASSERT (SCM_PORT_TYPE (port) == custom_port_type
              || SCM_PORT_TYPE (port) == custom_port_type_with_close_on_gc,
              port, SCM_ARG1, "custom port");
  return SCM_PACK (SCM_STREAM (port));
}

#undef FUNC_NAME

static void
scm_init_custom_ports (void)
{
#define RESOLVE_VAR(c_name, scm_name) \
  c_name##_var = scm_c_lookup ("custom-port-" scm_name);
  FOR_EACH_METHOD (RESOLVE_VAR);
#undef RESOlVE_VAR

  custom_port_type = scm_make_port_type ("custom-port", NULL, NULL);
  custom_port_type_with_close_on_gc =
    scm_make_port_type ("custom-port", NULL, NULL);

#define INIT_PORT_TYPE(c_name, scm_name)                                \
  scm_set_port_##c_name (custom_port_type, custom_port_##c_name);       \
  scm_set_port_##c_name (custom_port_type_with_close_on_gc,             \
                         custom_port_##c_name);
  FOR_EACH_METHOD_EXCEPT_READ_WRITE (INIT_PORT_TYPE);
#undef INIT_PORT_TYPE

  scm_set_port_scm_read (custom_port_type, scm_variable_ref (read_var));
  scm_set_port_scm_write (custom_port_type, scm_variable_ref (write_var));
  scm_set_port_scm_read (custom_port_type_with_close_on_gc,
                         scm_variable_ref (read_var));
  scm_set_port_scm_write (custom_port_type_with_close_on_gc,
                          scm_variable_ref (write_var));

  scm_set_port_needs_close_on_gc (custom_port_type_with_close_on_gc, 1);

#include "custom-ports.x"
}

void
scm_register_custom_ports (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_custom_ports",
                            (scm_t_extension_init_func) scm_init_custom_ports,
                            NULL);
}
