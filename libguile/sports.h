#ifndef SCM_SPORTS_H
#define SCM_SPORTS_H

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



#include "libguile/ports.h"



SCM_INTERNAL int scm_use_stream_ports;
SCM_INTERNAL SCM scm_i_make_stream_port (int id);
SCM_INTERNAL SCM scm_stream_port_p (SCM port);
SCM_INTERNAL SCM scm_make_stream_port (SCM fd);
SCM_INTERNAL void scm_init_stream_ports (void);

#endif
