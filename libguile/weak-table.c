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

#include "libguile/bdw-gc.h"
#include <gc/gc_mark.h>

#include "libguile/_scm.h"
#include "libguile/hash.h"
#include "libguile/eval.h"
#include "libguile/ports.h"

#include "libguile/validate.h"
#include "libguile/weak-table.h"


/* Weak Tables

   This file implements weak hash tables.  Weak hash tables are
   generally used when you want to augment some object with additional
   data, but when you don't have space to store the data in the object.
   For example, procedure properties are implemented with weak tables.

   Weak tables are implemented using an open-addressed hash table.
   Basically this means that there is an array of entries, and the item
   is expected to be found the slot corresponding to its hash code,
   modulo the length of the array.

   Collisions are handled using linear probing with the Robin Hood
   technique.  See Pedro Celis' paper, "Robin Hood Hashing":

     http://www.cs.uwaterloo.ca/research/tr/1986/CS-86-14.pdf

   The vector of entries is allocated in such a way that the GC doesn't
   trace the weak values.  For doubly-weak tables, this means that the
   entries are allocated as an "atomic" piece of memory.  Key-weak and
   value-weak tables use a special GC kind with a custom mark procedure.
   When items are added weakly into table, we attach a finalizer onto
   them that will remove them from the table if they become unreachable.

   An entry in the table consists of the key and the value, together
   with the hash code of the key.  We munge hash codes so that they are
   never 0.  In this way we can detect removed entries (key of zero but
   nonzero hash code), and can then reshuffle elements as needed to
   maintain the robin hood ordering.

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
        finalizers can rnu during allocation, we need to be very careful
        when allocating memory, because it might modify the table we are
        working on.
  */


typedef struct {
  unsigned long hash;
  scm_t_bits key;
  scm_t_bits value;
} scm_t_weak_entry;

typedef struct {
  scm_t_weak_entry *entries;    /* the data */
  scm_i_pthread_mutex_t lock;   /* the lock */
  scm_t_weak_table_kind kind;   /* what kind of table it is */
  unsigned long size;    	/* total number of slots. */
  unsigned long n_items;	/* number of items in table */
  unsigned long lower;		/* when to shrink */
  unsigned long upper;		/* when to grow */
  int size_index;		/* index into hashtable_size */
  int min_size_index;		/* minimum size_index */
} scm_t_weak_table;


#define SCM_WEAK_TABLE_P(x) (SCM_HAS_TYP7 (x, scm_tc7_weak_table))
#define SCM_VALIDATE_WEAK_TABLE(pos, arg) \
  SCM_MAKE_VALIDATE_MSG (pos, arg, WEAK_TABLE_P, "weak-table")
#define SCM_WEAK_TABLE(x) ((scm_t_weak_table *) SCM_CELL_WORD_1 (x))




static void
lock_weak_table (scm_t_weak_table *table)
{
  scm_i_pthread_mutex_lock (&table->lock);
}

static void
unlock_weak_table (scm_t_weak_table *table)
{
  scm_i_pthread_mutex_unlock (&table->lock);
}



static void weak_table_remove_x (scm_t_weak_table *table, unsigned long hash,
                                 scm_t_table_predicate_fn pred, void *closure);

static int
key_eq_predicate (SCM x, SCM y, void *closure)
{
  return scm_is_eq (x, SCM_PACK_POINTER (closure));
}

static int
value_eq_predicate (SCM x, SCM y, void *closure)
{
  return scm_is_eq (y, SCM_PACK_POINTER (closure));
}

static void
key_eq_finalizer (void *k, void *t)
{
  SCM key = SCM_PACK_POINTER (k);
  scm_t_weak_table *table = t;
  weak_table_remove_x (table, scm_ihashq (key, -1),
                       key_eq_predicate, SCM_UNPACK_POINTER (key));
}

struct finalizer_data {
  scm_t_weak_table *table;
  unsigned long hash;
};

static struct finalizer_data *
make_finalizer_data (scm_t_weak_table *t, unsigned long hash)
{
  struct finalizer_data *data;
  data = scm_gc_malloc (sizeof (*data), "weak table finalizer");
  data->table = t;
  data->hash = hash >> 1;
  return data;
}

static void
key_finalizer (void *obj, void *data)
{
  struct finalizer_data *d = data;
  weak_table_remove_x (d->table, d->hash, key_eq_predicate, obj);
}

static void
value_finalizer (void *obj, void *data)
{
  struct finalizer_data *d = data;
  weak_table_remove_x (d->table, d->hash, value_eq_predicate, obj);
}

static void
register_finalizers_for_key (scm_t_weak_table *t, unsigned long hash,
                             SCM key)
{
  if (t->kind == SCM_WEAK_TABLE_KIND_VALUE)
    return;

  if (!SCM_HEAP_OBJECT_P (key))
    return;

  if (t->kind == SCM_WEAK_TABLE_KIND_KEY
      && hash == ((scm_ihashq (key, -1) << 1) | 0x1))
    scm_i_add_finalizer (SCM2PTR (key), key_eq_finalizer, t);
  else
    scm_i_add_finalizer (SCM2PTR (key), key_finalizer,
                         make_finalizer_data (t, hash));
}

static void
register_finalizers_for_value (scm_t_weak_table *t, unsigned long hash,
                               SCM value)
{
  if (t->kind == SCM_WEAK_TABLE_KIND_KEY)
    return;

  if (!SCM_HEAP_OBJECT_P (value))
    return;

  scm_i_add_finalizer (SCM2PTR (value), value_finalizer,
                       make_finalizer_data (t, hash));
}



static void
copy_weak_entry (scm_t_weak_entry *from, scm_t_weak_entry *to)
{
  to->hash = from->hash;
  to->key = from->key;
  to->value = from->value;
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
rob_from_rich (scm_t_weak_table *table, unsigned long k)
{
  unsigned long empty, size;

  size = table->size;

  /* If we are to free up slot K in the table, we need room to do so.  */
  assert (table->n_items < size);
  
  empty = k;
  do 
    empty = (empty + 1) % size;
  while (table->entries[empty].hash);

  do
    {
      unsigned long last = empty ? (empty - 1) : (size - 1);
      copy_weak_entry (&table->entries[last], &table->entries[empty]);
      empty = last;
    }
  while (empty != k);

  table->entries[empty].hash = 0;
  table->entries[empty].key = 0;
  table->entries[empty].value = 0;
}

static void
give_to_poor (scm_t_weak_table *table, unsigned long k)
{
  /* Slot K was just freed up; possibly shuffle others down.  */
  unsigned long size = table->size;

  while (1)
    {
      unsigned long next = (k + 1) % size;
      unsigned long hash;

      hash = table->entries[next].hash;

      if (!hash || hash_to_index (hash, size) == next)
        break;

      copy_weak_entry (&table->entries[next], &table->entries[k]);

      k = next;
    }

  /* We have shuffled down any entries that should be shuffled down; now
     free the end.  */
  table->entries[k].hash = 0;
  table->entries[k].key = 0;
  table->entries[k].value = 0;
}




/* The GC "kinds" for singly-weak tables.  */
static int weak_key_gc_kind;
static int weak_value_gc_kind;

static struct GC_ms_entry *
mark_weak_key_table (GC_word *addr, struct GC_ms_entry *mark_stack_ptr,
                     struct GC_ms_entry *mark_stack_limit, GC_word env)
{
  scm_t_weak_entry *entries = (scm_t_weak_entry*) addr;
  unsigned long k, size = GC_size (addr) / sizeof (scm_t_weak_entry);

  for (k = 0; k < size; k++)
    if (entries[k].hash && entries[k].key)
      {
        SCM value = SCM_PACK (entries[k].value);
        mark_stack_ptr = GC_MARK_AND_PUSH ((GC_word*) SCM2PTR (value),
                                           mark_stack_ptr, mark_stack_limit,
                                           NULL);
      }

  return mark_stack_ptr;
}

static struct GC_ms_entry *
mark_weak_value_table (GC_word *addr, struct GC_ms_entry *mark_stack_ptr,
                       struct GC_ms_entry *mark_stack_limit, GC_word env)
{
  scm_t_weak_entry *entries = (scm_t_weak_entry*) addr;
  unsigned long k, size = GC_size (addr) / sizeof (scm_t_weak_entry);

  for (k = 0; k < size; k++)
    if (entries[k].hash && entries[k].value)
      {
        SCM key = SCM_PACK (entries[k].key);
        mark_stack_ptr = GC_MARK_AND_PUSH ((GC_word*) SCM2PTR (key),
                                           mark_stack_ptr, mark_stack_limit,
                                           NULL);
      }

  return mark_stack_ptr;
}

static scm_t_weak_entry *
allocate_entries (unsigned long size, scm_t_weak_table_kind kind)
{
  scm_t_weak_entry *ret;
  size_t bytes = size * sizeof (*ret);

  switch (kind)
    {
    case SCM_WEAK_TABLE_KIND_KEY:
      ret = GC_generic_malloc (bytes, weak_key_gc_kind);
      break;
    case SCM_WEAK_TABLE_KIND_VALUE:
      ret = GC_generic_malloc (bytes, weak_value_gc_kind);
      break;
    case SCM_WEAK_TABLE_KIND_BOTH:
      ret = scm_gc_malloc_pointerless (bytes, "weak-table");
      break;
    default:
      abort ();
    }

  memset (ret, 0, bytes);

  return ret;
}



/* Growing or shrinking is triggered when the load factor
 *
 *   L = N / S    (N: number of items in table, S: bucket vector length)
 *
 * passes an upper limit of 0.9 or a lower limit of 0.2.
 *
 * The implementation stores the upper and lower number of items which
 * trigger a resize in the hashtable object.
 *
 * Possible hash table sizes (primes) are stored in the array
 * hashtable_size.
 */

static unsigned long hashtable_size[] = {
  31, 61, 113, 223, 443, 883, 1759, 3517, 7027, 14051, 28099, 56197, 112363,
  224717, 449419, 898823, 1797641, 3595271, 7190537, 14381041, 28762081,
  57524111, 115048217, 230096423
};

#define HASHTABLE_SIZE_N (sizeof(hashtable_size)/sizeof(unsigned long))

static int
compute_size_index (scm_t_weak_table *table)
{
  int i = table->size_index;

  if (table->n_items < table->lower)
    {
      /* rehashing is not triggered when i <= min_size */
      do
	--i;
      while (i > table->min_size_index
	     && table->n_items < hashtable_size[i] / 5);
    }
  else if (table->n_items > table->upper)
    {
      ++i;
      if (i >= HASHTABLE_SIZE_N)
        /* The biggest size currently is 230096423, which for a 32-bit
           machine will occupy 2.3GB of memory at a load of 80%.  There
           is probably something better to do here, but if you have a
           weak map of that size, you are hosed in any case.  */
        abort ();
    }

  return i;
}

static int
is_acceptable_size_index (scm_t_weak_table *table, int size_index)
{
  int computed = compute_size_index (table);

  if (size_index == computed)
    /* We were going to grow or shrink, and allocating the new vector
       didn't change the target size.  */
    return 1;

  if (size_index == computed + 1)
    {
      /* We were going to enlarge the table, but allocating the new
         vector finalized some objects, making an enlargement
         unnecessary.  It might still be a good idea to use the larger
         table, though.  (This branch also gets hit if, while allocating
         the vector, some other thread was actively removing items from
         the table.  That is less likely, though.)  */
      unsigned long new_lower = hashtable_size[size_index] / 5;

      return table->size > new_lower;
    }

  if (size_index == computed - 1)
    {
      /* We were going to shrink the table, but when we dropped the lock
         to allocate the new vector, some other thread added elements to
         the table.  */
      return 0;
    }

  /* The computed size differs from our newly allocated size by more
     than one size index -- recalculate.  */
  return 0;
}

static void
resize_table (scm_t_weak_table *table)
{
  scm_t_weak_entry *old_entries, *new_entries;
  int new_size_index;
  unsigned long old_size, new_size, old_k;

  do 
    {
      new_size_index = compute_size_index (table);
      if (new_size_index == table->size_index)
        return;
      new_size = hashtable_size[new_size_index];
      unlock_weak_table (table);
      /* Allocating memory might cause finalizers to run, which could
         run anything, so drop our lock to avoid deadlocks.  */
      new_entries = allocate_entries (new_size, table->kind);
      lock_weak_table (table);
    }
  while (!is_acceptable_size_index (table, new_size_index));

  old_entries = table->entries;
  old_size = table->size;
  
  table->size_index = new_size_index;
  table->size = new_size;
  if (new_size_index <= table->min_size_index)
    table->lower = 0;
  else
    table->lower = new_size / 5;
  table->upper = 9 * new_size / 10;
  table->n_items = 0;
  table->entries = new_entries;

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
              rob_from_rich (table, new_k);
              break;
            }
        }
          
      table->n_items++;
      copy_weak_entry (&old_entries[old_k], &new_entries[new_k]);
    }
}




static SCM
weak_table_ref (scm_t_weak_table *table, unsigned long hash,
                scm_t_table_predicate_fn pred, void *closure,
                SCM dflt)
{
  unsigned long k, distance, size;
  scm_t_weak_entry *entries;
  
  hash = (hash << 1) | 0x1;

  lock_weak_table (table);

 retry:
  size = table->size;
  entries = table->entries;
  k = hash_to_index (hash, size);
  
  for (distance = 0; distance < size; distance++, k = (k + 1) % size)
    {
      unsigned long other_hash = entries[k].hash;

      if (!other_hash)
        /* Not found. */
        break;

      if (hash == other_hash)
        {
          SCM key = SCM_PACK (entries[k].key);
          SCM value = SCM_PACK (entries[k].value);

          unlock_weak_table (table);
          if (pred (key, value, closure))
            /* Found. */
            return value;
          lock_weak_table (table);

          if (table->entries != entries || table->size != size
              || !scm_is_eq (SCM_PACK (entries[k].key), key))
            /* The predicate caused the table to be changed. */
            goto retry;

          continue;
        }

      /* If the entry's distance is less, our key is not in the table.  */
      if (entry_distance (other_hash, k, size) < distance)
        break;
    }

  unlock_weak_table (table);
  return dflt;
}


static void
weak_table_put_x (scm_t_weak_table *table, unsigned long hash,
                  scm_t_table_predicate_fn pred, void *closure,
                  SCM key, SCM value)
{
  unsigned long k, distance, size;
  scm_t_weak_entry *entries;
  
  hash = (hash << 1) | 0x1;

  lock_weak_table (table);

 retry:
  size = table->size;
  entries = table->entries;
  k = hash_to_index (hash, size);

  for (distance = 0; ; distance++, k = (k + 1) % size)
    {
      unsigned long other_hash = entries[k].hash;

      if (!other_hash)
        /* Found an empty entry. */
        break;

      if (hash == other_hash)
        {
          SCM prev_key = SCM_PACK (entries[k].key);
          SCM prev_value = SCM_PACK (entries[k].value);
          int have_match;

          unlock_weak_table (table);
          have_match = pred (prev_key, prev_value, closure);
          lock_weak_table (table);

          if (table->entries != entries || table->size != size
              || !scm_is_eq (SCM_PACK (entries[k].key), prev_key))
            /* The predicate caused the table to be changed. */
            goto retry;

          if (!have_match)
            /* No match, keep looking. */
            continue;
          
          /* We have a match; update the table and add finalizers if
             needed. */
          entries[k].key = SCM_UNPACK (key);
          entries[k].value = SCM_UNPACK (value);

          unlock_weak_table (table);

          if (!scm_is_eq (prev_key, key))
            register_finalizers_for_key (table, hash, key);
          if (!scm_is_eq (prev_value, value))
            register_finalizers_for_value (table, hash, value);

          return;
        }

      if (table->n_items > table->upper)
        /* Full table, time to resize.  */
        {
          resize_table (table);
          goto retry;
        }

      /* Displace the entry if our distance is less, otherwise keep
         looking.  */
      if (entry_distance (other_hash, k, size) < distance)
        {
          rob_from_rich (table, k);
          break;
        }
    }
          
  /* Insert a new entry.  */
  table->n_items++;
  entries[k].hash = hash;
  entries[k].key = SCM_UNPACK (key);
  entries[k].value = SCM_UNPACK (value);

  unlock_weak_table (table);

  register_finalizers_for_key (table, hash, key);
  register_finalizers_for_value (table, hash, value);
}


static void
weak_table_remove_x (scm_t_weak_table *table, unsigned long hash,
                   scm_t_table_predicate_fn pred, void *closure)
{
  unsigned long k, distance, size;
  scm_t_weak_entry *entries;
  
  hash = (hash << 1) | 0x1;

  lock_weak_table (table);

 retry:
  size = table->size;
  entries = table->entries;
  k = hash_to_index (hash, size);

  for (distance = 0; distance < size; distance++, k = (k + 1) % size)
    {
      unsigned long other_hash = entries[k].hash;

      if (!other_hash)
        /* Not found. */
        break;

      if (hash == other_hash)
        {
          SCM prev_key = SCM_PACK (entries[k].key);
          SCM prev_value = SCM_PACK (entries[k].value);
          int have_match;

          unlock_weak_table (table);
          have_match = pred (prev_key, prev_value, closure);
          lock_weak_table (table);

          if (table->entries != entries || table->size != size
              || !scm_is_eq (SCM_PACK (entries[k].key), prev_key))
            /* The predicate caused the table to be changed. */
            goto retry;

          if (!have_match)
            /* No match, keep looking. */
            continue;
          
          /* We have a match; remove the entry.  */
          entries[k].hash = 0;
          entries[k].key = 0;
          entries[k].value = 0;

          if (--table->n_items < table->lower)
            resize_table (table);
          else
            give_to_poor (table, k);

          break;
        }

      /* If the entry's distance is less, our key is not in the table.  */
      if (entry_distance (other_hash, k, size) < distance)
        break;
    }

  unlock_weak_table (table);
}




/* A weak table of weak tables, for use in the pthread_atfork handler. */
static SCM all_weak_tables = SCM_BOOL_F;

#if SCM_USE_PTHREAD_THREADS

static void
lock_all_weak_tables (void)
{
  scm_t_weak_table *s;
  scm_t_weak_entry *entries;
  unsigned long k, size;

  s = SCM_WEAK_TABLE (all_weak_tables);
  lock_weak_table (s);
  size = s->size;
  entries = s->entries;

  for (k = 0; k < size; k++)
    if (entries[k].hash)
      lock_weak_table (SCM_WEAK_TABLE (SCM_PACK (entries[k].key)));
}

static void
unlock_all_weak_tables (void)
{
  scm_t_weak_table *s;
  scm_t_weak_entry *entries;
  unsigned long k, size;

  s = SCM_WEAK_TABLE (all_weak_tables);
  size = s->size;
  entries = s->entries;

  for (k = 0; k < size; k++)
    if (entries[k].hash)
      unlock_weak_table (SCM_WEAK_TABLE (SCM_PACK (entries[k].key)));

  unlock_weak_table (s);
}

#endif /* SCM_USE_PTHREAD_THREADS */




static SCM
make_weak_table (unsigned long k, scm_t_weak_table_kind kind)
{
  scm_t_weak_table *table;
  SCM ret;

  int i = 0, n = k ? k : 31;
  while (i + 1 < HASHTABLE_SIZE_N && n > hashtable_size[i])
    ++i;
  n = hashtable_size[i];

  table = scm_gc_malloc (sizeof (*table), "weak-table");
  table->entries = allocate_entries (n, kind);
  table->kind = kind;
  table->n_items = 0;
  table->size = n;
  table->lower = 0;
  table->upper = 9 * n / 10;
  table->size_index = i;
  table->min_size_index = i;
  scm_i_pthread_mutex_init (&table->lock, NULL);

  ret = scm_cell (scm_tc7_weak_table, (scm_t_bits)table);

  if (scm_is_true (all_weak_tables))
    scm_weak_table_putq_x (all_weak_tables, ret, SCM_BOOL_T);
  
  return ret;
}

void
scm_i_weak_table_print (SCM exp, SCM port, scm_print_state *pstate)
{
  scm_puts_unlocked ("#<", port);
  scm_puts_unlocked ("weak-table ", port);
  scm_uintprint (SCM_WEAK_TABLE (exp)->n_items, 10, port);
  scm_putc_unlocked ('/', port);
  scm_uintprint (SCM_WEAK_TABLE (exp)->size, 10, port);
  scm_puts_unlocked (">", port);
}

SCM
scm_c_make_weak_table (unsigned long k, scm_t_weak_table_kind kind)
{
  return make_weak_table (k, kind);
}

SCM
scm_weak_table_p (SCM obj)
{
  return scm_from_bool (SCM_WEAK_TABLE_P (obj));
}

SCM
scm_c_weak_table_ref (SCM table, unsigned long raw_hash,
                      scm_t_table_predicate_fn pred,
                      void *closure, SCM dflt)
#define FUNC_NAME "weak-table-ref"
{
  scm_t_weak_table *t;

  SCM_VALIDATE_WEAK_TABLE (1, table);

  t = SCM_WEAK_TABLE (table);

  return weak_table_ref (t, raw_hash, pred, closure, dflt);
}
#undef FUNC_NAME

void
scm_c_weak_table_put_x (SCM table, unsigned long raw_hash,
                        scm_t_table_predicate_fn pred,
                        void *closure, SCM key, SCM value)
#define FUNC_NAME "weak-table-put!"
{
  scm_t_weak_table *t;

  SCM_VALIDATE_WEAK_TABLE (1, table);

  t = SCM_WEAK_TABLE (table);

  weak_table_put_x (t, raw_hash, pred, closure, key, value);
}
#undef FUNC_NAME

void
scm_c_weak_table_remove_x (SCM table, unsigned long raw_hash,
                           scm_t_table_predicate_fn pred,
                           void *closure)
#define FUNC_NAME "weak-table-remove!"
{
  scm_t_weak_table *t;

  SCM_VALIDATE_WEAK_TABLE (1, table);

  t = SCM_WEAK_TABLE (table);

  weak_table_remove_x (t, raw_hash, pred, closure);
}
#undef FUNC_NAME

SCM
scm_weak_table_refq (SCM table, SCM key, SCM dflt)
{
  if (SCM_UNBNDP (dflt))
    dflt = SCM_BOOL_F;
  
  return scm_c_weak_table_ref (table, scm_ihashq (key, -1),
                               key_eq_predicate, SCM_UNPACK_POINTER (key),
                               dflt);
}

SCM
scm_weak_table_putq_x (SCM table, SCM key, SCM value)
{
  scm_c_weak_table_put_x (table, scm_ihashq (key, -1),
                          key_eq_predicate, SCM_UNPACK_POINTER (key),
                          key, value);
  return SCM_UNSPECIFIED;
}

SCM
scm_weak_table_remq_x (SCM table, SCM key)
{
  scm_c_weak_table_remove_x (table, scm_ihashq (key, -1),
                             key_eq_predicate, SCM_UNPACK_POINTER (key));
  return SCM_UNSPECIFIED;
}

SCM
scm_weak_table_clear_x (SCM table)
#define FUNC_NAME "weak-table-clear!"
{
  scm_t_weak_table *t;

  SCM_VALIDATE_WEAK_TABLE (1, table);

  t = SCM_WEAK_TABLE (table);

  lock_weak_table (t);

  memset (t->entries, 0, sizeof (scm_t_weak_entry) * t->size);
  t->n_items = 0;

  unlock_weak_table (t);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_c_weak_table_fold (scm_t_table_fold_fn proc, void *closure,
                       SCM init, SCM table)
{
  scm_t_weak_table *t;
  scm_t_weak_entry *entries;
  unsigned long k, size;

  t = SCM_WEAK_TABLE (table);

  lock_weak_table (t);

  size = t->size;
  entries = t->entries;

  for (k = 0; k < size; k++)
    {
      if (entries[k].hash)
        {
          SCM key, value;

          key = SCM_PACK (entries[k].key);
          value = SCM_PACK (entries[k].value);
          
          /* Release table lock while we call the function.  */
          unlock_weak_table (t);
          init = proc (closure, key, value, init);
          lock_weak_table (t);
          if (entries != t->entries)
            /* Nothing sensible to do here; just break out.  */
            break;
        }
    }
  
  unlock_weak_table (t);
  
  return init;
}

static SCM
fold_trampoline (void *closure, SCM k, SCM v, SCM init)
{
  return scm_call_3 (SCM_PACK_POINTER (closure), k, v, init);
}

SCM
scm_weak_table_fold (SCM proc, SCM init, SCM table)
#define FUNC_NAME "weak-table-fold"
{
  SCM_VALIDATE_WEAK_TABLE (3, table);
  SCM_VALIDATE_PROC (1, proc);

  return scm_c_weak_table_fold (fold_trampoline, SCM_UNPACK_POINTER (proc), init, table);
}
#undef FUNC_NAME

static SCM
for_each_trampoline (void *closure, SCM k, SCM v, SCM seed)
{
  scm_call_2 (SCM_PACK_POINTER (closure), k, v);
  return seed;
}

SCM
scm_weak_table_for_each (SCM proc, SCM table)
#define FUNC_NAME "weak-table-for-each"
{
  SCM_VALIDATE_WEAK_TABLE (2, table);
  SCM_VALIDATE_PROC (1, proc);

  scm_c_weak_table_fold (for_each_trampoline, SCM_UNPACK_POINTER (proc), SCM_BOOL_F, table);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

static SCM
map_trampoline (void *closure, SCM k, SCM v, SCM seed)
{
  return scm_cons (scm_call_2 (SCM_PACK_POINTER (closure), k, v), seed);
}

SCM
scm_weak_table_map_to_list (SCM proc, SCM table)
#define FUNC_NAME "weak-table-map->list"
{
  SCM_VALIDATE_WEAK_TABLE (2, table);
  SCM_VALIDATE_PROC (1, proc);

  return scm_c_weak_table_fold (map_trampoline, SCM_UNPACK_POINTER (proc), SCM_EOL, table);
}
#undef FUNC_NAME




/* Legacy interface.  */

SCM_DEFINE (scm_make_weak_key_hash_table, "make-weak-key-hash-table", 0, 1, 0, 
	    (SCM n),
	    "@deffnx {Scheme Procedure} make-weak-value-hash-table size\n"
	    "@deffnx {Scheme Procedure} make-doubly-weak-hash-table size\n"
	    "Return a weak hash table with @var{size} buckets.\n"
	    "\n"
	    "You can modify weak hash tables in exactly the same way you\n"
	    "would modify regular hash tables. (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_key_hash_table
{
  return scm_c_make_weak_table (SCM_UNBNDP (n) ? 0 : scm_to_ulong (n),
                                SCM_WEAK_TABLE_KIND_KEY);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_weak_value_hash_table, "make-weak-value-hash-table", 0, 1, 0, 
            (SCM n),
	    "Return a hash table with weak values with @var{size} buckets.\n"
	    "(@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_weak_value_hash_table
{
  return scm_c_make_weak_table (SCM_UNBNDP (n) ? 0 : scm_to_ulong (n),
                                SCM_WEAK_TABLE_KIND_VALUE);
}
#undef FUNC_NAME


SCM_DEFINE (scm_make_doubly_weak_hash_table, "make-doubly-weak-hash-table", 1, 0, 0, 
            (SCM n),
	    "Return a hash table with weak keys and values with @var{size}\n"
	    "buckets.  (@pxref{Hash Tables})")
#define FUNC_NAME s_scm_make_doubly_weak_hash_table
{
  return scm_c_make_weak_table (SCM_UNBNDP (n) ? 0 : scm_to_ulong (n),
                                SCM_WEAK_TABLE_KIND_BOTH);
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_key_hash_table_p, "weak-key-hash-table?", 1, 0, 0, 
           (SCM obj),
	    "@deffnx {Scheme Procedure} weak-value-hash-table? obj\n"
	    "@deffnx {Scheme Procedure} doubly-weak-hash-table? obj\n"
	    "Return @code{#t} if @var{obj} is the specified weak hash\n"
	    "table. Note that a doubly weak hash table is neither a weak key\n"
	    "nor a weak value hash table.")
#define FUNC_NAME s_scm_weak_key_hash_table_p
{
  return scm_from_bool (SCM_WEAK_TABLE_P (obj) &&
                        SCM_WEAK_TABLE (obj)->kind == SCM_WEAK_TABLE_KIND_KEY);
}
#undef FUNC_NAME


SCM_DEFINE (scm_weak_value_hash_table_p, "weak-value-hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a weak value hash table.")
#define FUNC_NAME s_scm_weak_value_hash_table_p
{
  return scm_from_bool (SCM_WEAK_TABLE_P (obj) &&
                        SCM_WEAK_TABLE (obj)->kind == SCM_WEAK_TABLE_KIND_VALUE);
}
#undef FUNC_NAME


SCM_DEFINE (scm_doubly_weak_hash_table_p, "doubly-weak-hash-table?", 1, 0, 0, 
            (SCM obj),
	    "Return @code{#t} if @var{obj} is a doubly weak hash table.")
#define FUNC_NAME s_scm_doubly_weak_hash_table_p
{
  return scm_from_bool (SCM_WEAK_TABLE_P (obj) &&
                        SCM_WEAK_TABLE (obj)->kind == SCM_WEAK_TABLE_KIND_BOTH);
}
#undef FUNC_NAME





void
scm_weak_table_prehistory (void)
{
  weak_key_gc_kind =
    GC_new_kind (GC_new_free_list (),
		 GC_MAKE_PROC (GC_new_proc (mark_weak_key_table), 0),
		 0, 0);
  weak_value_gc_kind =
    GC_new_kind (GC_new_free_list (),
		 GC_MAKE_PROC (GC_new_proc (mark_weak_value_table), 0),
		 0, 0);

#if SCM_USE_PTHREAD_THREADS
  all_weak_tables = scm_c_make_weak_table (0, SCM_WEAK_TABLE_KIND_KEY);
  pthread_atfork (lock_all_weak_tables, unlock_all_weak_tables,
                  unlock_all_weak_tables);
#endif
}

void
scm_init_weak_table ()
{
#include "libguile/weak-table.x"
}

/*
  Local Variables:
  c-file-style: "gnu"
  End:
*/
