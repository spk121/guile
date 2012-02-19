/* Copyright (C) 2011, 2012 Free Software Foundation, Inc.
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



#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>

#include "libguile/_scm.h"
#include "libguile/hash.h"
#include "libguile/eval.h"
#include "libguile/ports.h"
#include "libguile/bdw-gc.h"

#include "libguile/validate.h"
#include "libguile/weak-set.h"


/* Weak Sets

   This file implements weak sets.  One example of a weak set is the
   symbol table, where you want all instances of the `foo' symbol to map
   to one object.  So when you load a file and it wants a symbol with
   the characters "foo", you one up in the table, using custom hash and
   equality predicates.  Only if one is not found will you bother to
   cons one up and intern it.

   Another use case for weak sets is the set of open ports.  Guile needs
   to be able to flush them all when the process exits, but the set
   shouldn't prevent the GC from collecting the port (and thus closing
   it).

   Weak sets are implemented using an open-addressed hash table.
   Basically this means that there is an array of entries, and the item
   is expected to be found the slot corresponding to its hash code,
   modulo the length of the array.

   Collisions are handled using linear probing with the Robin Hood
   technique.  See Pedro Celis' paper, "Robin Hood Hashing":

     http://www.cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf

   The vector of entries is allocated as an "atomic" piece of memory, so
   that the GC doesn't trace it.  When an item is added to the set, a
   finalizer is added linking the item and the set.  If the item becomes
   unreachable, then the finalizer will remove it from the table.

   An entry is not just an item, though; the hash code is also stored in
   the entry.  We munge hash codes so that they are never 0.  In this
   way we can detect removed entries (key of zero but nonzero hash
   code), and can then reshuffle elements as needed to maintain the
   robin hood ordering.

   Compared to buckets-and-chains hash tables, open addressing has the
   advantage that it is very cache-friendly.  It also uses less memory.

   Implementation-wise, there are two things to note.

     1. We assume that hash codes are evenly distributed across the
        range of unsigned longs.  The actual hash code stored in the
        entry is left-shifted by 1 bit (losing 1 bit of hash precision),
        and then or'd with 1.  In this way we ensure that the hash field
        of an occupied entry is nonzero.  To map to an index, we
        right-shift the hash by one, divide by the size, and take the
        remainder.

     2. Since the weak references are cleared using finalizers, and
        finalizers can run during allocation, we need to be very careful
        when allocating memory, because it might modify the set we are
        working on.
*/


typedef struct {
  unsigned long hash;
  scm_t_bits key;
} scm_t_weak_entry;

typedef struct {
  scm_t_weak_entry *entries;    /* the data */
  scm_i_pthread_mutex_t lock;   /* the lock */
  unsigned long size;    	/* total number of slots. */
  unsigned long n_items;	/* number of items in set */
  unsigned long lower;		/* when to shrink */
  unsigned long upper;		/* when to grow */
  int size_index;		/* index into hashset_size */
  int min_size_index;		/* minimum size_index */
} scm_t_weak_set;


#define SCM_WEAK_SET_P(x) (SCM_HAS_TYP7 (x, scm_tc7_weak_set))
#define SCM_VALIDATE_WEAK_SET(pos, arg) \
  SCM_MAKE_VALIDATE_MSG (pos, arg, WEAK_SET_P, "weak-set")
#define SCM_WEAK_SET(x) ((scm_t_weak_set *) SCM_CELL_WORD_1 (x))




static void
lock_weak_set (scm_t_weak_set *set)
{
  scm_i_pthread_mutex_lock (&set->lock);
}

static void
unlock_weak_set (scm_t_weak_set *set)
{
  scm_i_pthread_mutex_unlock (&set->lock);
}



static void weak_set_remove_x (scm_t_weak_set *set, unsigned long hash,
                               scm_t_set_predicate_fn pred, void *closure);

static int
eq_predicate (SCM x, void *closure)
{
  return scm_is_eq (x, SCM_PACK_POINTER (closure));
}

static void
eq_finalizer (void *k, void *s)
{
  SCM key = SCM_PACK_POINTER (k);
  scm_t_weak_set *set = s;
  weak_set_remove_x (set, scm_ihashq (key, -1), eq_predicate, k);
}

static void
symbol_finalizer (void *k, void *s)
{
  SCM key = SCM_PACK_POINTER (k);
  scm_t_weak_set *set = s;
  weak_set_remove_x (set, scm_i_symbol_hash (key), eq_predicate, k);
}

struct finalizer_data {
  scm_t_weak_set *set;
  unsigned long hash;
};

static struct finalizer_data *
make_finalizer_data (scm_t_weak_set *s, unsigned long hash)
{
  struct finalizer_data *data;
  data = scm_gc_malloc (sizeof (*data), "weak set finalizer");
  data->set = s;
  data->hash = hash >> 1;
  return data;
}

static void
equal_finalizer (void *obj, void *data)
{
  struct finalizer_data *d = data;
  weak_set_remove_x (d->set, d->hash, eq_predicate, obj);
}

static void
register_finalizer (scm_t_weak_set *s, unsigned long hash, SCM key)
{
  if (!SCM_HEAP_OBJECT_P (key))
    return;

  if (hash == ((scm_ihashq (key, -1) << 1) | 0x1))
    scm_i_add_finalizer (SCM2PTR (key), eq_finalizer, s);
  else if (scm_is_symbol (key) && hash == ((scm_i_symbol_hash (key) << 1) | 0x1))
    /* Hack for the symbol table, whose hashes are the string hashes.  */
    scm_i_add_finalizer (SCM2PTR (key), symbol_finalizer, s);
  else
    scm_i_add_finalizer (SCM2PTR (key), equal_finalizer,
                         make_finalizer_data (s, hash));
}



static void
copy_weak_entry (scm_t_weak_entry *src, scm_t_weak_entry *dst)
{
  dst->hash = src->hash;
  dst->key = src->key;
}
  
static unsigned long
hash_to_index (unsigned long hash, unsigned long size)
{
  return (hash >> 1) % size;
}

static unsigned long
entry_distance (unsigned long hash, unsigned long k, unsigned long size)
{
  unsigned long origin = hash_to_index (hash, size);

  if (k >= origin)
    return k - origin;
  else
    /* The other key was displaced and wrapped around.  */
    return size - origin + k;
}

static void
rob_from_rich (scm_t_weak_set *set, unsigned long k)
{
  unsigned long empty, size;

  size = set->size;

  /* If we are to free up slot K in the set, we need room to do so.  */
  assert (set->n_items < size);
  
  empty = k;
  do 
    empty = (empty + 1) % size;
  while (set->entries[empty].key);

  do
    {
      unsigned long last = empty ? (empty - 1) : (size - 1);
      copy_weak_entry (&set->entries[last], &set->entries[empty]);
      empty = last;
    }
  while (empty != k);

  /* Just for sanity.  */
  set->entries[empty].hash = 0;
  set->entries[empty].key = 0;
}

static void
give_to_poor (scm_t_weak_set *set, unsigned long k)
{
  /* Slot K was just freed up; possibly shuffle others down.  */
  unsigned long size = set->size;

  while (1)
    {
      unsigned long next = (k + 1) % size;
      unsigned long hash;

      hash = set->entries[next].hash;

      if (!hash || hash_to_index (hash, size) == next)
        break;

      copy_weak_entry (&set->entries[next], &set->entries[k]);

      k = next;
    }

  /* We have shuffled down any entries that should be shuffled down; now
     free the end.  */
  set->entries[k].hash = 0;
  set->entries[k].key = 0;
}




/* Growing or shrinking is triggered when the load factor
 *
 *   L = N / S    (N: number of items in set, S: bucket vector length)
 *
 * passes an upper limit of 0.9 or a lower limit of 0.2.
 *
 * The implementation stores the upper and lower number of items which
 * trigger a resize in the hashset object.
 *
 * Possible hash set sizes (primes) are stored in the array
 * hashset_size.
 */

static unsigned long hashset_size[] = {
  31, 61, 113, 223, 443, 883, 1759, 3517, 7027, 14051, 28099, 56197, 112363,
  224717, 449419, 898823, 1797641, 3595271, 7190537, 14381041, 28762081,
  57524111, 115048217, 230096423
};

#define HASHSET_SIZE_N (sizeof(hashset_size)/sizeof(unsigned long))

static int
compute_size_index (scm_t_weak_set *set)
{
  int i = set->size_index;

  if (set->n_items < set->lower)
    {
      /* rehashing is not triggered when i <= min_size */
      do
	--i;
      while (i > set->min_size_index
	     && set->n_items < hashset_size[i] / 5);
    }
  else if (set->n_items > set->upper)
    {
      ++i;
      if (i >= HASHSET_SIZE_N)
        /* The biggest size currently is 230096423, which for a 32-bit
           machine will occupy 1.5GB of memory at a load of 80%.  There
           is probably something better to do here, but if you have a
           weak map of that size, you are hosed in any case.  */
        abort ();
    }

  return i;
}

static int
is_acceptable_size_index (scm_t_weak_set *set, int size_index)
{
  int computed = compute_size_index (set);

  if (size_index == computed)
    /* We were going to grow or shrink, and allocating the new vector
       didn't change the target size.  */
    return 1;

  if (size_index == computed + 1)
    {
      /* We were going to enlarge the set, but allocating the new
         vector finalized some objects, making an enlargement
         unnecessary.  It might still be a good idea to use the larger
         set, though.  (This branch also gets hit if, while allocating
         the vector, some other thread was actively removing items from
         the set.  That is less likely, though.)  */
      unsigned long new_lower = hashset_size[size_index] / 5;

      return set->size > new_lower;
    }

  if (size_index == computed - 1)
    {
      /* We were going to shrink the set, but when we dropped the lock
         to allocate the new vector, some other thread added elements to
         the set.  */
      return 0;
    }

  /* The computed size differs from our newly allocated size by more
     than one size index -- recalculate.  */
  return 0;
}

static void
resize_set (scm_t_weak_set *set)
{
  scm_t_weak_entry *old_entries, *new_entries;
  int new_size_index;
  unsigned long old_size, new_size, old_k;

  do 
    {
      new_size_index = compute_size_index (set);
      if (new_size_index == set->size_index)
        return;
      new_size = hashset_size[new_size_index];
      unlock_weak_set (set);
      /* Allocating memory might cause finalizers to run, which could
         run anything, so drop our lock to avoid deadlocks.  */
      new_entries = scm_gc_malloc_pointerless (new_size * sizeof(scm_t_weak_entry),
                                               "weak set");
      lock_weak_set (set);
    }
  while (!is_acceptable_size_index (set, new_size_index));

  old_entries = set->entries;
  old_size = set->size;

  memset (new_entries, 0, new_size * sizeof(scm_t_weak_entry));

  set->size_index = new_size_index;
  set->size = new_size;
  if (new_size_index <= set->min_size_index)
    set->lower = 0;
  else
    set->lower = new_size / 5;
  set->upper = 9 * new_size / 10;
  set->n_items = 0;
  set->entries = new_entries;

  for (old_k = 0; old_k < old_size; old_k++)
    {
      unsigned long new_k, distance;

      if (!old_entries[old_k].hash)
        continue;
      
      new_k = hash_to_index (old_entries[old_k].hash, new_size);

      for (distance = 0; ; distance++, new_k = (new_k + 1) % new_size)
        {
          unsigned long other_hash = new_entries[new_k].hash;

          if (!other_hash)
            /* Found an empty entry. */
            break;

          /* Displace the entry if our distance is less, otherwise keep
             looking. */
          if (entry_distance (other_hash, new_k, new_size) < distance)
            {
              rob_from_rich (set, new_k);
              break;
            }
        }
          
      set->n_items++;
      copy_weak_entry (&old_entries[old_k], &new_entries[new_k]);
    }
}



static SCM
weak_set_lookup (scm_t_weak_set *set, unsigned long hash,
                 scm_t_set_predicate_fn pred, void *closure,
                 SCM dflt)
{
  unsigned long k, distance, size;
  scm_t_weak_entry *entries;
  
  lock_weak_set (set);

  size = set->size;
  entries = set->entries;

  hash = (hash << 1) | 0x1;
  k = hash_to_index (hash, size);
  
  for (distance = 0; distance < size; distance++, k = (k + 1) % size)
    {
      unsigned long other_hash = entries[k].hash;

      if (!other_hash)
        /* Not found. */
        break;

      if (hash == other_hash
          && pred (SCM_PACK (entries[k].key), closure))
        /* Found. */
        {
          SCM ret = SCM_PACK (entries[k].key);
          unlock_weak_set (set);
          return ret;
        }

      /* If the entry's distance is less, our key is not in the set.  */
      if (entry_distance (other_hash, k, size) < distance)
        break;
    }

  unlock_weak_set (set);
  return dflt;
}


static SCM
weak_set_add_x (scm_t_weak_set *set, unsigned long hash,
                scm_t_set_predicate_fn pred, void *closure,
                SCM obj)
{
  unsigned long k, distance, size;
  scm_t_weak_entry *entries;
  
  lock_weak_set (set);

  hash = (hash << 1) | 0x1;

 retry:
  size = set->size;
  entries = set->entries;
  k = hash_to_index (hash, size);

  for (distance = 0; ; distance++, k = (k + 1) % size)
    {
      unsigned long other_hash = entries[k].hash;

      if (!other_hash)
        /* Found an empty entry. */
        break;

      if (other_hash == hash
          && pred (SCM_PACK (entries[k].key), closure))
        /* Found an entry with this key. */
        {
          SCM ret = SCM_PACK (entries[k].key);
          unlock_weak_set (set);
          return ret;
        }

      if (set->n_items > set->upper)
        /* Full set, time to resize.  */
        {
          resize_set (set);
          goto retry;
        }

      /* Displace the entry if our distance is less, otherwise keep
         looking. */
      if (entry_distance (other_hash, k, size) < distance)
        {
          rob_from_rich (set, k);
          break;
        }
    }
          
  set->n_items++;
  entries[k].hash = hash;
  entries[k].key = SCM_UNPACK (obj);

  unlock_weak_set (set);
  register_finalizer (set, hash, obj);

  return obj;
}


static void
weak_set_remove_x (scm_t_weak_set *set, unsigned long hash,
                   scm_t_set_predicate_fn pred, void *closure)
{
  unsigned long k, distance, size;
  scm_t_weak_entry *entries;
  
  lock_weak_set (set);

  size = set->size;
  entries = set->entries;

  hash = (hash << 1) | 0x1;
  k = hash_to_index (hash, size);

  for (distance = 0; distance < size; distance++, k = (k + 1) % size)
    {
      unsigned long other_hash = entries[k].hash;

      if (!other_hash)
        /* Not found. */
        break;

      if (other_hash == hash
          && pred (SCM_PACK (entries[k].key), closure))
        /* Found an entry with this key. */
        {
          entries[k].hash = 0;
          entries[k].key = 0;

          if (--set->n_items < set->lower)
            resize_set (set);
          else
            give_to_poor (set, k);

          break;
        }

      /* If the entry's distance is less, our key is not in the set.  */
      if (entry_distance (other_hash, k, size) < distance)
        break;
    }

  unlock_weak_set (set);
}




/* A weak set of weak sets, for use in the pthread_atfork handler. */
static SCM all_weak_sets = SCM_BOOL_F;

#if SCM_USE_PTHREAD_THREADS

static void
lock_all_weak_sets (void)
{
  scm_t_weak_set *s;
  scm_t_weak_entry *entries;
  unsigned long k, size;

  s = SCM_WEAK_SET (all_weak_sets);
  lock_weak_set (s);
  size = s->size;
  entries = s->entries;

  for (k = 0; k < size; k++)
    if (entries[k].hash)
      lock_weak_set (SCM_WEAK_SET (SCM_PACK (entries[k].key)));
}

static void
unlock_all_weak_sets (void)
{
  scm_t_weak_set *s;
  scm_t_weak_entry *entries;
  unsigned long k, size;

  s = SCM_WEAK_SET (all_weak_sets);
  size = s->size;
  entries = s->entries;

  for (k = 0; k < size; k++)
    if (entries[k].hash)
      unlock_weak_set (SCM_WEAK_SET (SCM_PACK (entries[k].key)));
  
  unlock_weak_set (s);
}

#endif /* SCM_USE_PTHREAD_THREADS */




static SCM
make_weak_set (unsigned long k)
{
  scm_t_weak_set *set;

  int i = 0, n = k ? k : 31;
  while (i + 1 < HASHSET_SIZE_N && n > hashset_size[i])
    ++i;
  n = hashset_size[i];

  set = scm_gc_malloc (sizeof (*set), "weak-set");
  set->entries = scm_gc_malloc_pointerless (n * sizeof(scm_t_weak_entry),
                                            "weak-set");
  memset (set->entries, 0, n * sizeof(scm_t_weak_entry));
  set->n_items = 0;
  set->size = n;
  set->lower = 0;
  set->upper = 9 * n / 10;
  set->size_index = i;
  set->min_size_index = i;
  scm_i_pthread_mutex_init (&set->lock, NULL);

  return scm_cell (scm_tc7_weak_set, (scm_t_bits)set);
}

void
scm_i_weak_set_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts_unlocked ("#<", port);
  scm_puts_unlocked ("weak-set ", port);
  scm_uintprint (SCM_WEAK_SET (exp)->n_items, 10, port);
  scm_putc_unlocked ('/', port);
  scm_uintprint (SCM_WEAK_SET (exp)->size, 10, port);
  scm_puts_unlocked (">", port);
}

SCM
scm_c_make_weak_set (unsigned long k)
{
  SCM ret;

  ret = make_weak_set (k);

  if (scm_is_true (all_weak_sets))
    scm_weak_set_add_x (all_weak_sets, ret);

  return ret;
}

SCM
scm_weak_set_p (SCM obj)
{
  return scm_from_bool (SCM_WEAK_SET_P (obj));
}

SCM
scm_weak_set_clear_x (SCM set)
{
  scm_t_weak_set *s = SCM_WEAK_SET (set);

  lock_weak_set (s);

  memset (s->entries, 0, sizeof (scm_t_weak_entry) * s->size);
  s->n_items = 0;

  unlock_weak_set (s);

  return SCM_UNSPECIFIED;
}

SCM
scm_c_weak_set_lookup (SCM set, unsigned long raw_hash,
                       scm_t_set_predicate_fn pred,
                       void *closure, SCM dflt)
{
  scm_t_weak_set *s = SCM_WEAK_SET (set);

  return weak_set_lookup (s, raw_hash, pred, closure, dflt);
}

SCM
scm_c_weak_set_add_x (SCM set, unsigned long raw_hash,
                      scm_t_set_predicate_fn pred,
                      void *closure, SCM obj)
{
  scm_t_weak_set *s = SCM_WEAK_SET (set);

  return weak_set_add_x (s, raw_hash, pred, closure, obj);
}

void
scm_c_weak_set_remove_x (SCM set, unsigned long raw_hash,
                         scm_t_set_predicate_fn pred,
                         void *closure)
{
  scm_t_weak_set *s = SCM_WEAK_SET (set);

  weak_set_remove_x (s, raw_hash, pred, closure);
}

SCM
scm_weak_set_add_x (SCM set, SCM obj)
{
  return scm_c_weak_set_add_x (set, scm_ihashq (obj, -1),
                               eq_predicate, SCM_UNPACK_POINTER (obj), obj);
}

SCM
scm_weak_set_remove_x (SCM set, SCM obj)
{
  scm_c_weak_set_remove_x (set, scm_ihashq (obj, -1),
                           eq_predicate, SCM_UNPACK_POINTER (obj));

  return SCM_UNSPECIFIED;
}

SCM
scm_c_weak_set_fold (scm_t_set_fold_fn proc, void *closure,
                     SCM init, SCM set)
{
  scm_t_weak_set *s;
  scm_t_weak_entry *entries;
  unsigned long k, size;

  s = SCM_WEAK_SET (set);

  lock_weak_set (s);

  size = s->size;
  entries = s->entries;

  for (k = 0; k < size; k++)
    {
      if (entries[k].hash)
        {
          /* Release set lock while we call the function.  */
          SCM key = SCM_PACK (entries[k].key);
          unlock_weak_set (s);
          init = proc (closure, key, init);
          lock_weak_set (s);
          if (entries != s->entries)
            /* Nothing sensible to do here; just break out.  */
            break;
        }
    }
  
  unlock_weak_set (s);
  
  return init;
}

static SCM
fold_trampoline (void *closure, SCM item, SCM init)
{
  return scm_call_2 (SCM_PACK_POINTER (closure), item, init);
}

SCM
scm_weak_set_fold (SCM proc, SCM init, SCM set)
{
  return scm_c_weak_set_fold (fold_trampoline, SCM_UNPACK_POINTER (proc), init, set);
}

static SCM
for_each_trampoline (void *closure, SCM item, SCM seed)
{
  scm_call_1 (SCM_PACK_POINTER (closure), item);
  return seed;
}

SCM
scm_weak_set_for_each (SCM proc, SCM set)
{
  scm_c_weak_set_fold (for_each_trampoline, SCM_UNPACK_POINTER (proc), SCM_BOOL_F, set);

  return SCM_UNSPECIFIED;
}

static SCM
map_trampoline (void *closure, SCM item, SCM seed)
{
  return scm_cons (scm_call_1 (SCM_PACK_POINTER (closure), item), seed);
}

SCM
scm_weak_set_map_to_list (SCM proc, SCM set)
{
  return scm_c_weak_set_fold (map_trampoline, SCM_UNPACK_POINTER (proc), SCM_EOL, set);
}




void
scm_weak_set_prehistory (void)
{
#if SCM_USE_PTHREAD_THREADS
  all_weak_sets = scm_c_make_weak_set (0);
  pthread_atfork (lock_all_weak_sets, unlock_all_weak_sets, unlock_all_weak_sets);
#endif
}

void
scm_init_weak_set ()
{
#include "libguile/weak-set.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
