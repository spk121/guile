/* Copyright 1995-1996,1998-2003,2006,2009-2011,2013,2018,2023
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
# include <config.h>
#endif

#include "eval.h"
#include "modules.h"
#include "threads.h"
#include "variable.h"

#include "vports.h"




static SCM make_soft_port_var;

static void
init_make_soft_port_var (void)
{
  make_soft_port_var =
    scm_c_public_variable ("ice-9 soft-ports", "make-soft-port");
}

SCM
scm_make_soft_port (SCM pv, SCM modes)
{
  static scm_i_pthread_once_t once = SCM_I_PTHREAD_ONCE_INIT;
  scm_i_pthread_once (&once, init_make_soft_port_var);

  return scm_call_2 (scm_variable_ref (make_soft_port_var), pv, modes);
}
