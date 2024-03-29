/* Copyright 1995-1996,1998-2001,2003-2004,2006,2009-2013,2015,2018
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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include "async.h"
#include "bdw-gc.h"
#include "finalizers.h"
#include "goops.h"
#include "gsubr.h"
#include "instructions.h"
#include "numbers.h"
#include "ports.h"
#include "programs.h"

#include "smob.h"

#include <gc/gc_mark.h>




/* scm_smobs scm_numsmob
 * implement a fixed sized array of smob records.
 * Indexes into this table are used when generating type
 * tags for smobjects (if you know a tag you can get an index and conversely).
 */

#define MAX_SMOB_COUNT SCM_I_MAX_SMOB_TYPE_COUNT

long scm_numsmob;
scm_smob_descriptor scm_smobs[MAX_SMOB_COUNT];

void
scm_assert_smob_type (scm_t_bits tag, SCM val)
{
  if (!SCM_SMOB_PREDICATE (tag, val))
    scm_wrong_type_arg_msg (NULL, 0, val, scm_smobs[SCM_TC2SMOBNUM(tag)].name);
}

/* {Mark}
 */

/* This function is vestigial.  It used to be the mark function's
   responsibility to set the mark bit on the smob or port, but now the
   generic marking routine in gc.c takes care of that, and a zero
   pointer for a mark function means "don't bother".  So you never
   need scm_mark0.

   However, we leave it here because it's harmless to call it, and
   people out there have smob code that uses it, and there's no reason
   to make their links fail.  */

SCM 
scm_mark0 (SCM ptr SCM_UNUSED)
{
  return SCM_BOOL_F;
}

SCM 
/* Dirk::FIXME: The name markcdr is misleading, since the term cdr should only
   be used for real pairs. */
scm_markcdr (SCM ptr)
{
  return SCM_CELL_OBJECT_1 (ptr);
}


/* {Free}
 */

size_t 
scm_free0 (SCM ptr SCM_UNUSED)
{
  return 0;
}


/* {Print}
 */

int
scm_smob_print (SCM exp, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  long n = SCM_SMOBNUM (exp);
  scm_puts ("#<", port);
  scm_puts (SCM_SMOBNAME (n) ? SCM_SMOBNAME (n) : "smob", port);
  scm_putc (' ', port);
  if (scm_smobs[n].size)
    scm_uintprint (SCM_CELL_WORD_1 (exp), 16, port);
  else
    scm_uintprint (SCM_UNPACK (exp), 16, port);
  scm_putc ('>', port);
  return 1;
}


/* {Apply}
 */

static SCM scm_smob_trampolines[16];
  
/* (nargs * nargs) + nopt + rest * (nargs + 1) */
#define SCM_SMOB_TRAMPOLINE(nreq,nopt,rest) \
  scm_smob_trampolines[(nreq + nopt + rest) * (nreq + nopt + rest) \
                       + nopt + rest * (nreq + nopt + rest + 1)]

static SCM
apply_0 (SCM smob)
{
  SCM (*subr0)(SCM _smob) = SCM_SMOB_DESCRIPTOR (smob).apply;
  return subr0 (smob);
}

static SCM
apply_1 (SCM smob, SCM a)
{
  SCM (*subr1)(SCM _smob, SCM _a) = SCM_SMOB_DESCRIPTOR (smob).apply;
  return subr1 (smob, a);
}

static SCM
apply_2 (SCM smob, SCM a, SCM b)
{
  SCM (*subr2)(SCM _smob, SCM _a, SCM _b) = SCM_SMOB_DESCRIPTOR (smob).apply;
  return subr2 (smob, a, b);
}

static SCM
apply_3 (SCM smob, SCM a, SCM b, SCM c)
{
  SCM (*subr3)(SCM _smob, SCM _a, SCM _b, SCM _c)
    = SCM_SMOB_DESCRIPTOR (smob).apply;
  return subr3 (smob, a, b, c);
}

static SCM
scm_smob_trampoline (unsigned int nreq, unsigned int nopt,
                     unsigned int rest)
{
  SCM trampoline;

  if (SCM_UNLIKELY (rest > 1 || nreq + nopt + rest > 3))
    scm_out_of_range ("make-smob", scm_from_uint (nreq + nopt + rest));
      
  trampoline = SCM_SMOB_TRAMPOLINE (nreq, nopt, rest);

  if (SCM_LIKELY (SCM_UNPACK (trampoline)))
    return trampoline;

  switch (nreq + nopt + rest)
    {
      /* The + 1 is for the smob itself.  */
    case 0:
      trampoline = scm_c_make_gsubr ("apply-smob/0", nreq + 1, nopt, rest,
                                     apply_0);
      break;
    case 1:
      trampoline = scm_c_make_gsubr ("apply-smob/1", nreq + 1, nopt, rest,
                                     apply_1);
      break;
    case 2:
      trampoline = scm_c_make_gsubr ("apply-smob/2", nreq + 1, nopt, rest,
                                     apply_2);
      break;
    case 3:
      trampoline = scm_c_make_gsubr ("apply-smob/3", nreq + 1, nopt, rest,
                                     apply_3);
      break;
    default:
      abort ();
    }

  SCM_SMOB_TRAMPOLINE (nreq, nopt, rest) = trampoline;

  return trampoline;
}



scm_t_bits 
scm_make_smob_type (char const *name, size_t size)
#define FUNC_NAME "scm_make_smob_type"
{
  long new_smob;

  scm_i_pthread_mutex_lock (&scm_i_misc_mutex);
  new_smob = scm_numsmob;
  if (scm_numsmob != MAX_SMOB_COUNT)
    ++scm_numsmob;
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  if (new_smob == MAX_SMOB_COUNT)
    scm_misc_error (FUNC_NAME, "maximum number of smobs exceeded", SCM_EOL);

  scm_smobs[new_smob].name = name;
  scm_smobs[new_smob].size = size;

  /* Make a class object if Goops is present. */
  if (SCM_UNPACK (scm_i_smob_class[0]) != 0)
    scm_i_smob_class[new_smob] = scm_make_extended_class (name, 0);

  return scm_tc7_smob + new_smob * 256;
}
#undef FUNC_NAME


void
scm_set_smob_mark (scm_t_bits tc, SCM (*mark) (SCM))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].mark = mark;
}

void
scm_set_smob_free (scm_t_bits tc, size_t (*free) (SCM))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].free = free;
}

void
scm_set_smob_print (scm_t_bits tc, int (*print) (SCM, SCM, scm_print_state*))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].print = print;
}

void
scm_set_smob_equalp (scm_t_bits tc, SCM (*equalp) (SCM, SCM))
{
  scm_smobs[SCM_TC2SMOBNUM (tc)].equalp = equalp;
}

void
scm_set_smob_apply (scm_t_bits tc, SCM (*apply) (),
		    unsigned int req, unsigned int opt, unsigned int rst)
{
  SCM trampoline = scm_smob_trampoline (req, opt, rst);

  scm_smobs[SCM_TC2SMOBNUM (tc)].apply = apply;
  scm_smobs[SCM_TC2SMOBNUM (tc)].apply_trampoline = trampoline;

  if (SCM_UNPACK (scm_i_smob_class[0]) != 0)
    scm_i_inherit_applicable (scm_i_smob_class[SCM_TC2SMOBNUM (tc)]);
}

SCM
scm_make_smob (scm_t_bits tc)
{
  scm_t_bits n = SCM_TC2SMOBNUM (tc);
  size_t size = scm_smobs[n].size;
  scm_t_bits data = (size > 0
		     ? (scm_t_bits) scm_gc_malloc (size, SCM_SMOBNAME (n))
		     : 0);

  SCM_RETURN_NEWSMOB (tc, data);
}



/* Marking SMOBs using user-supplied mark procedures.  */


/* The GC kind used for SMOB types that provide a custom mark procedure.  */
static int smob_gc_kind;

/* Mark stack pointer and limit, used by `scm_gc_mark'.  */
static scm_i_pthread_key_t current_mark_stack_pointer;
static scm_i_pthread_key_t current_mark_stack_limit;


/* The generic SMOB mark procedure that gets called for SMOBs allocated
   with smob_gc_kind.  */
static struct GC_ms_entry *
smob_mark (GC_word *addr, struct GC_ms_entry *mark_stack_ptr,
	   struct GC_ms_entry *mark_stack_limit, GC_word env)
{
  register SCM cell;
  register scm_t_bits tc, smobnum;

  cell = SCM_PACK_POINTER (addr);

  if (SCM_TYP7 (cell) != scm_tc7_smob)
    /* It is likely that the GC passed us a pointer to a free-list element
       which we must ignore (see warning in `gc/gc_mark.h').  */
    return mark_stack_ptr;

  tc = SCM_CELL_WORD_0 (cell);
  smobnum = SCM_TC2SMOBNUM (tc);

  if (smobnum >= scm_numsmob)
    /* The first word looks corrupt.  */
    abort ();

  mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (SCM_CELL_OBJECT_1 (cell)),
				     mark_stack_ptr,
				     mark_stack_limit, NULL);
  mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (SCM_CELL_OBJECT_2 (cell)),
				     mark_stack_ptr,
				     mark_stack_limit, NULL);
  mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (SCM_CELL_OBJECT_3 (cell)),
				     mark_stack_ptr,
				     mark_stack_limit, NULL);

  if (scm_smobs[smobnum].mark)
    {
      SCM obj;

      scm_i_pthread_setspecific (current_mark_stack_pointer, mark_stack_ptr);
      scm_i_pthread_setspecific (current_mark_stack_limit, mark_stack_limit);

      /* Invoke the SMOB's mark procedure, which will in turn invoke
	 `scm_gc_mark', which may modify `current_mark_stack_pointer'.  */
      obj = scm_smobs[smobnum].mark (cell);

      mark_stack_ptr = scm_i_pthread_getspecific (current_mark_stack_pointer);

      if (SCM_HEAP_OBJECT_P (obj))
	/* Mark the returned object.  */
	mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (obj),
					   mark_stack_ptr,
					   mark_stack_limit, NULL);

      scm_i_pthread_setspecific (current_mark_stack_pointer, NULL);
      scm_i_pthread_setspecific (current_mark_stack_limit, NULL);
    }

  return mark_stack_ptr;

}

/* Mark object O.  We assume that this function is only called during the mark
   phase, i.e., from within `smob_mark' or one of its descendants.  */
void
scm_gc_mark (SCM o)
{
  if (SCM_HEAP_OBJECT_P (o))
    {
      void *mark_stack_ptr, *mark_stack_limit;

      mark_stack_ptr = scm_i_pthread_getspecific (current_mark_stack_pointer);
      mark_stack_limit = scm_i_pthread_getspecific (current_mark_stack_limit);

      if (mark_stack_ptr == NULL)
	/* The function was not called from a mark procedure.  */
	abort ();

      mark_stack_ptr = GC_MARK_AND_PUSH (SCM2PTR (o),
					 mark_stack_ptr, mark_stack_limit,
					 NULL);
      scm_i_pthread_setspecific (current_mark_stack_pointer, mark_stack_ptr);
    }
}


static void*
clear_smobnum (void *ptr)
{
  SCM smob;
  scm_t_bits smobnum;

  smob = SCM_PACK_POINTER (ptr);

  smobnum = SCM_SMOBNUM (smob);
  /* Frob the object's type in place, re-setting it to be the "finalized
     smob" type.  This will prevent other routines from accessing its
     internals in a way that assumes that the smob data is valid.  This
     is notably the case for SMOB's own "mark" procedure, if any; as the
     finalizer runs without the alloc lock, it's possible for a GC to
     occur while it's running, in which case the object is alive and yet
     its data is invalid.  */
  SCM_SET_SMOB_DATA_0 (smob, SCM_SMOB_DATA_0 (smob) & ~(scm_t_bits) 0xff00);

  return (void *) smobnum;
}

/* Finalize SMOB by calling its SMOB type's free function, if any.  */
static void
finalize_smob (void *ptr, void *data)
{
  SCM smob;
  scm_t_bits smobnum;
  size_t (* free_smob) (SCM);

  smob = SCM_PACK_POINTER (ptr);
  smobnum = (scm_t_bits) GC_call_with_alloc_lock (clear_smobnum, ptr);

#if 0
  printf ("finalizing SMOB %p (smobnum: %u)\n", ptr, smobnum);
#endif

  free_smob = scm_smobs[smobnum].free;
  if (free_smob)
    free_smob (smob);
}

/* Return a SMOB with typecode TC.  The SMOB type corresponding to TC may
   provide a custom mark procedure and it will be honored.  */
SCM
scm_i_new_smob (scm_t_bits tc, scm_t_bits data)
{
  scm_t_bits smobnum = SCM_TC2SMOBNUM (tc);
  SCM ret;

  /* Use the smob_gc_kind if needed to allow the mark procedure to
     run.  Since the marker only deals with double cells, that case
     allocates a double cell.  We leave words 2 and 3 to there initial
     values, which is 0.  */
  if (scm_smobs [smobnum].mark)
    ret = SCM_PACK_POINTER (GC_generic_malloc (2 * sizeof (scm_t_cell), smob_gc_kind));
  else
    ret = SCM_PACK_POINTER (GC_MALLOC (sizeof (scm_t_cell)));
  
  SCM_SET_CELL_WORD_1 (ret, data);
  SCM_SET_CELL_WORD_0 (ret, tc);

  if (scm_smobs[smobnum].free)
    scm_i_set_finalizer (SCM2PTR (ret), finalize_smob, NULL);

  return ret;
}

/* Return a SMOB with typecode TC.  The SMOB type corresponding to TC may
   provide a custom mark procedure and it will be honored.  */
SCM
scm_i_new_double_smob (scm_t_bits tc, scm_t_bits data1,
                       scm_t_bits data2, scm_t_bits data3)
{
  scm_t_bits smobnum = SCM_TC2SMOBNUM (tc);
  SCM ret;

  /* Use the smob_gc_kind if needed to allow the mark procedure to
     run.  */
  if (scm_smobs [smobnum].mark)
    ret = SCM_PACK_POINTER (GC_generic_malloc (2 * sizeof (scm_t_cell), smob_gc_kind));
  else
    ret = SCM_PACK_POINTER (GC_MALLOC (2 * sizeof (scm_t_cell)));
  
  SCM_SET_CELL_WORD_3 (ret, data3);
  SCM_SET_CELL_WORD_2 (ret, data2);
  SCM_SET_CELL_WORD_1 (ret, data1);
  SCM_SET_CELL_WORD_0 (ret, tc);

  if (scm_smobs[smobnum].free)
    scm_i_set_finalizer (SCM2PTR (ret), finalize_smob, NULL);

  return ret;
}




SCM
scm_smob_type_class (scm_t_bits tc)
{
  scm_load_goops ();

  return scm_i_smob_class[SCM_TC2SMOBNUM (tc)];
}



void
scm_smob_prehistory ()
{
  long i;
  scm_t_bits finalized_smob_tc16;

  scm_i_pthread_key_create (&current_mark_stack_pointer, NULL);
  scm_i_pthread_key_create (&current_mark_stack_limit, NULL);

  smob_gc_kind = GC_new_kind (GC_new_free_list (),
			      GC_MAKE_PROC (GC_new_proc (smob_mark), 0),
			      0,
			      /* Clear new objects.  As of version 7.1, libgc
				 doesn't seem to support passing 0 here.  */
			      1);

  scm_numsmob = 0;
  for (i = 0; i < MAX_SMOB_COUNT; ++i)
    {
      scm_smobs[i].name       = 0;
      scm_smobs[i].size       = 0;
      scm_smobs[i].mark       = 0;
      scm_smobs[i].free       = 0;
      scm_smobs[i].print      = scm_smob_print;
      scm_smobs[i].equalp     = 0;
      scm_smobs[i].apply      = 0;
      scm_smobs[i].apply_trampoline = SCM_BOOL_F;
    }

  finalized_smob_tc16 = scm_make_smob_type ("finalized smob", 0);
  if (SCM_TC2SMOBNUM (finalized_smob_tc16) != 0) abort ();
}
