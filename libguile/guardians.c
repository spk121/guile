/* Copyright (C) 1998,1999,2000,2001, 2006, 2008, 2009, 2011, 2012 Free Software Foundation, Inc.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */


/* This is an implementation of guardians as described in
 * R. Kent Dybvig, Carl Bruggeman, and David Eby (1993) "Guardians in
 * a Generation-Based Garbage Collector" ACM SIGPLAN Conference on
 * Programming Language Design and Implementation, June 1993
 * ftp://ftp.cs.indiana.edu/pub/scheme-repository/doc/pubs/guardians.ps.gz
 *
 * Original design:          Mikael Djurfeldt
 * Original implementation:  Michael Livshin
 * Hacked on since by:       everybody
 *
 * By this point, the semantics are actually quite different from
 * those described in the abovementioned paper.  The semantic changes
 * are there to improve safety and intuitiveness.  The interface is
 * still (mostly) the one described by the paper, however.
 *
 * Boiled down again:        Marius Vollmer
 *
 * Now they should again behave like those described in the paper.
 * Scheme guardians should be simple and friendly, not like the greedy
 * monsters we had...
 *
 * Rewritten for the Boehm-Demers-Weiser GC by Ludovic Courtès.
 * FIXME: This is currently not thread-safe.
 */

/* Uncomment the following line to debug guardian finalization.  */
/* #define DEBUG_GUARDIANS 1 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "libguile/_scm.h"
#include "libguile/async.h"
#include "libguile/ports.h"
#include "libguile/print.h"
#include "libguile/smob.h"
#include "libguile/validate.h"
#include "libguile/root.h"
#include "libguile/hashtab.h"
#include "libguile/deprecation.h"
#include "libguile/eval.h"

#include "libguile/guardians.h"
#include "libguile/bdw-gc.h"

static scm_i_pthread_mutex_t guardian_lock = SCM_I_PTHREAD_MUTEX_INITIALIZER;
SCM_PTHREAD_ATFORK_LOCK_STATIC_MUTEX (guardian_lock);

/* A weak-key table mapping guarded values to the guardians that guard
   them.  The values are themselves weak tables: guardian -> count.  */
static SCM guarded_values;

static void finalize_guarded (void *obj, void *data);

static void
register_guarded_value (SCM val, SCM guardian)
{
  SCM t, n;
  int new_value = 0;

  scm_i_pthread_mutex_lock (&guardian_lock);
  t = scm_weak_table_refq (guarded_values, val, SCM_BOOL_F);
  if (scm_is_false (t))
    {
      new_value = 1;
      t = scm_c_make_weak_table (0, SCM_WEAK_TABLE_KIND_BOTH);
      scm_weak_table_putq_x (guarded_values, val, t);
    }
  n = scm_weak_table_refq (t, guardian, SCM_INUM0);
  scm_weak_table_putq_x (t, guardian, scm_oneplus (n));
  scm_i_pthread_mutex_unlock (&guardian_lock);

  if (new_value)
    scm_i_add_resuscitator (SCM_UNPACK_POINTER (val), finalize_guarded,
                            SCM_UNPACK_POINTER (t));
}

static SCM
for_each_guardian (void *closure, SCM guardian, SCM n, SCM val)
{
  void (*callback) (SCM, SCM) = closure;
  for (; scm_is_true (scm_positive_p (n)); n = scm_oneminus (n))
    callback (val, guardian);
  return val;
}

static void
unregister_guarded_value (SCM val, SCM guardians,
                          void (*callback) (SCM val, SCM guardian))
{
  scm_weak_table_remq_x (guarded_values, val);
  scm_c_weak_table_fold (for_each_guardian, callback, val, guardians);
}

static scm_t_bits tc16_guardian;

typedef struct t_guardian
{
  unsigned long live;
  SCM zombies;
} t_guardian;

#define GUARDIAN_P(x)    SCM_SMOB_PREDICATE(tc16_guardian, x)
#define GUARDIAN_DATA(x) ((t_guardian *) SCM_SMOB_DATA_1 (x))


static int
guardian_print (SCM guardian, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  t_guardian *g = GUARDIAN_DATA (guardian);
  
  scm_puts_unlocked ("#<guardian ", port);
  scm_uintprint ((scm_t_bits) g, 16, port);

  scm_puts_unlocked (" (reachable: ", port);
  scm_display (scm_from_uint (g->live), port);
  scm_puts_unlocked (" unreachable: ", port);
  scm_display (scm_length (g->zombies), port);
  scm_puts_unlocked (")", port);

  scm_puts_unlocked (">", port);

  return 1;
}

static void
add_zombie (SCM val, SCM guardian)
{
  t_guardian *g = GUARDIAN_DATA (guardian);

  g->zombies = scm_cons (val, g->zombies);
}

static void
finalize_guarded (void *obj, void *data)
{
  unregister_guarded_value (SCM_PACK_POINTER (obj), SCM_PACK_POINTER (data),
                            add_zombie);
}

/* Add OBJ as a guarded object of GUARDIAN.  */
static void
scm_i_guard (SCM guardian, SCM obj)
{
  t_guardian *g = GUARDIAN_DATA (guardian);

  if (!SCM_HEAP_OBJECT_P (obj))
    return;

  register_guarded_value (obj, guardian);
  g->live++;
}

static SCM
scm_i_get_one_zombie (SCM guardian)
{
  t_guardian *g = GUARDIAN_DATA (guardian);
  SCM res = SCM_BOOL_F;

  if (!scm_is_null (g->zombies))
    {
      /* Note: We return zombies in reverse order.  */
      res = SCM_CAR (g->zombies);
      g->zombies = SCM_CDR (g->zombies);
    }

  return res;
}

/* This is the Scheme entry point for each guardian: If OBJ is an
 * object, it's added to the guardian's live list.  If OBJ is unbound,
 * the next available unreachable object (or #f if none) is returned.
 *
 * If the second optional argument THROW_P is true (the default), then
 * an error is raised if GUARDIAN is greedy and OBJ is already greedily
 * guarded.  If THROW_P is false, #f is returned instead of raising the
 * error, and #t is returned if everything is fine.
 */ 
static SCM
guardian_apply (SCM guardian, SCM obj, SCM throw_p)
{
  if (!SCM_UNBNDP (obj))
    {
      scm_i_guard (guardian, obj);
      return SCM_UNSPECIFIED;
    }
  else
    return scm_i_get_one_zombie (guardian);
}

SCM_DEFINE (scm_make_guardian, "make-guardian", 0, 0, 0, 
	    (),
"Create a new guardian.  A guardian protects a set of objects from\n"
"garbage collection, allowing a program to apply cleanup or other\n"
"actions.\n"
"\n"
"@code{make-guardian} returns a procedure representing the guardian.\n"
"Calling the guardian procedure with an argument adds the argument to\n"
"the guardian's set of protected objects.  Calling the guardian\n"
"procedure without an argument returns one of the protected objects\n"
"which are ready for garbage collection, or @code{#f} if no such object\n"
"is available.  Objects which are returned in this way are removed from\n"
"the guardian.\n"
"\n"
"You can put a single object into a guardian more than once and you can\n"
"put a single object into more than one guardian.  The object will then\n"
"be returned multiple times by the guardian procedures.\n"
"\n"
"An object is eligible to be returned from a guardian when it is no\n"
"longer referenced from outside any guardian.\n"
"\n"
"There is no guarantee about the order in which objects are returned\n"
"from a guardian.  If you want to impose an order on finalization\n"
"actions, for example, you can do that by keeping objects alive in some\n"
"global data structure until they are no longer needed for finalizing\n"
"other objects.\n"
"\n"
"Being an element in a weak vector, a key in a hash table with weak\n"
"keys, or a value in a hash table with weak value does not prevent an\n"
"object from being returned by a guardian.  But as long as an object\n"
"can be returned from a guardian it will not be removed from such a\n"
"weak vector or hash table.  In other words, a weak link does not\n"
"prevent an object from being considered collectable, but being inside\n"
"a guardian prevents a weak link from being broken.\n"
"\n"
"A key in a weak key hash table can be though of as having a strong\n"
"reference to its associated value as long as the key is accessible.\n"
"Consequently, when the key only accessible from within a guardian, the\n"
"reference from the key to the value is also considered to be coming\n"
"from within a guardian.  Thus, if there is no other reference to the\n"
	    "value, it is eligible to be returned from a guardian.\n")
#define FUNC_NAME s_scm_make_guardian
{
  t_guardian *g = scm_gc_malloc (sizeof (t_guardian), "guardian");
  SCM z;

  g->live = 0;
  g->zombies = SCM_EOL;

  SCM_NEWSMOB (z, tc16_guardian, g);

  return z;
}
#undef FUNC_NAME

void
scm_init_guardians ()
{
  /* We use unordered finalization `a la Java.  */
  GC_java_finalization = 1;

  tc16_guardian = scm_make_smob_type ("guardian", 0);

  scm_set_smob_print (tc16_guardian, guardian_print);
  scm_set_smob_apply (tc16_guardian, guardian_apply, 0, 1, 0);

  guarded_values = scm_c_make_weak_table (0, SCM_WEAK_TABLE_KIND_KEY);

#include "libguile/guardians.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
