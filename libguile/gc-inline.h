#ifndef SCM_GC_INLINE_H
#define SCM_GC_INLINE_H

/* Copyright 1995-1996,1998-2004,2006-2014,2018-2019
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

/* Much of this file was copied from gc_inline.h, from the BDW
 * collector.  Its copyright notice is:
 *
 * Copyright 1988, 1989 Hans-J. Boehm, Alan J. Demers
 * Copyright 1991-1995 by Xerox Corporation.  All rights reserved.
 * Copyright 2005 Hewlett-Packard Development Company, L.P.
 *
 * THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY EXPRESSED
 * OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.
 *
 * Permission is hereby granted to use or copy this program
 * for any purpose,  provided the above notices are retained on all copies.
 * Permission to modify the code and to distribute modified code is granted,
 * provided the above notices are retained, and a notice that the code was
 * modified is included with the above copyright notice.
 */



#include "libguile/gc.h"
#include "libguile/bdw-gc.h"
#include "libguile/threads.h"

#include <gc/gc_inline.h> /* GC_generic_malloc_many */


static inline size_t
scm_inline_gc_bytes_to_freelist_index (size_t bytes)
{
  return (bytes - 1U) / SCM_INLINE_GC_GRANULE_BYTES;
}

static inline size_t
scm_inline_gc_freelist_object_size (size_t idx)
{
  return (idx + 1U) * SCM_INLINE_GC_GRANULE_BYTES;
}

static inline void *
scm_inline_gc_alloc (void **freelist, size_t idx, int kind)
{
  void *head = *freelist;

  if (SCM_UNLIKELY (!head))
    {
      size_t bytes = scm_inline_gc_freelist_object_size (idx);
      GC_generic_malloc_many (bytes, kind, freelist);
      head = *freelist;
      if (SCM_UNLIKELY (!head))
        return (*GC_get_oom_fn ()) (bytes);
    }

  *freelist = *(void **)(head);

  return head;
}

static inline void *
scm_inline_gc_malloc_pointerless (scm_thread *thread, size_t bytes)
{
  size_t idx = scm_inline_gc_bytes_to_freelist_index (bytes);

  if (SCM_UNLIKELY (idx >= SCM_INLINE_GC_FREELIST_COUNT))
    return GC_malloc_atomic (bytes);

  return scm_inline_gc_alloc
    (&thread->pointerless_freelists[idx], idx, GC_I_PTRFREE);
}

static inline void *
scm_inline_gc_malloc (scm_thread *thread, size_t bytes)
{
  size_t idx = scm_inline_gc_bytes_to_freelist_index (bytes);

  if (SCM_UNLIKELY (idx >= SCM_INLINE_GC_FREELIST_COUNT))
    return GC_malloc (bytes);

  return scm_inline_gc_alloc
    (&thread->freelists[idx], idx, GC_I_NORMAL);
}

static inline void *
scm_inline_gc_malloc_words (scm_thread *thread, size_t words)
{
  return scm_inline_gc_malloc (thread, words * sizeof (void *));
}

static inline void *
scm_inline_gc_malloc_pointerless_words (scm_thread *thread, size_t words)
{
  return scm_inline_gc_malloc_pointerless (thread, words * sizeof (void *));
}

static inline SCM
scm_inline_cons (scm_thread *thread, SCM x, SCM y)
{
  SCM cell = SCM_PACK_POINTER (scm_inline_gc_malloc_words (thread, 2));
  
  SCM_GC_SET_CELL_WORD (cell, 0, SCM_UNPACK (x));
  SCM_GC_SET_CELL_WORD (cell, 1, SCM_UNPACK (y));

  return cell;
}

#endif  /* SCM_GC_INLINE_H */
