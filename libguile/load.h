#ifndef SCM_LOAD_H
#define SCM_LOAD_H

/* Copyright 1995-1996,1998,2000-2001,2006,2008-2011,2013,2018
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



#include "libguile/scm.h"


SCM_API SCM scm_parse_path (SCM path, SCM tail);
SCM_API SCM scm_parse_path_with_ellipsis (SCM path, SCM base);
SCM_API SCM scm_primitive_load (SCM filename);
SCM_API SCM scm_c_primitive_load (const char *filename);
SCM_API SCM scm_sys_package_data_dir (void);
SCM_API SCM scm_sys_library_dir (void);
SCM_API SCM scm_sys_site_dir (void);
SCM_INTERNAL SCM scm_sys_extensions_dir (void);
SCM_INTERNAL SCM scm_sys_ccache_dir (void);
SCM_API SCM scm_sys_global_site_dir (void);
SCM_API SCM scm_sys_site_ccache_dir (void);
SCM_API SCM scm_search_path (SCM path, SCM filename, SCM rest);
SCM_API SCM scm_sys_search_load_path (SCM filename);
SCM_API SCM scm_primitive_load_path (SCM filename_and_exception_on_not_found);
SCM_API SCM scm_c_primitive_load_path (const char *filename);
SCM_INTERNAL SCM scm_sys_warn_auto_compilation_enabled (void);
SCM_INTERNAL void scm_init_load_path (void);
SCM_INTERNAL void scm_init_load (void);
SCM_INTERNAL void scm_init_load_should_auto_compile (void);
SCM_INTERNAL void scm_init_eval_in_scheme (void);
SCM_INTERNAL char *scm_i_mirror_backslashes (char *path);

#endif  /* SCM_LOAD_H */
