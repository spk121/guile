#ifndef SCM_BDW_GC_H
#define SCM_BDW_GC_H

/* Copyright 2006,2008-2009,2011-2014,2018
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

/* Correct header inclusion.  */

#include "libguile/scmconfig.h"

#if SCM_USE_PTHREAD_THREADS

/* When pthreads are used, let `libgc' know about it and redirect allocation
   calls such as `GC_MALLOC ()' to (contention-free, faster) thread-local
   allocation.  */

# define GC_THREADS 1

/* Don't #define pthread routines to their GC_pthread counterparts.
   Instead we will be careful inside Guile to use the GC_pthread
   routines.  */
# define GC_NO_THREAD_REDIRECTS 1

#ifdef __MINGW32__
/* Rely on pthreads-w32.  */
#define GC_WIN32_PTHREADS
#endif

#endif

#include <gc/gc.h>

/* Return true if PTR points to the heap.  */
#define SCM_I_IS_POINTER_TO_THE_HEAP(ptr)	\
  (GC_base (ptr) != NULL)

/* Register a disappearing link for the object pointed to by OBJ such that
   the pointer pointed to be LINK is cleared when OBJ is reclaimed.  Do so
   only if OBJ actually points to the heap.  See
   http://thread.gmane.org/gmane.comp.programming.garbage-collection.boehmgc/2563
   for details.  */
#define SCM_I_REGISTER_DISAPPEARING_LINK(link, obj)		\
  ((SCM_I_IS_POINTER_TO_THE_HEAP (obj))				\
   ? GC_GENERAL_REGISTER_DISAPPEARING_LINK ((link), (obj))	\
   : 0)


#endif /* SCM_BDW_GC_H */
