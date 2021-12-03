/* Copyright 1995-2016,2018-2021
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

#include <stdlib.h>
#include <stdio.h>
#include <verify.h>

#include "numbers.h"

#include "integers.h"

/* Some functions that use GMP's mpn functions assume that a
   non-negative fixnum will always fit in a 'mp_limb_t'.  */
verify (SCM_MOST_POSITIVE_FIXNUM <= (mp_limb_t) -1);

#define NLIMBS_MAX (SSIZE_MAX / sizeof(mp_limb_t))

#ifndef NDEBUG
#define ASSERT(x)                                                       \
  do {                                                                  \
    if (!(x))                                                           \
      {                                                                 \
        fprintf (stderr, "%s:%d: assertion failed\n", __FILE__, __LINE__); \
        abort();                                                        \
      }                                                                 \
  } while (0)
#else
#define ASSERT(x) do { } while (0)
#endif

struct scm_bignum
{
  scm_t_bits tag;
  /* FIXME: In Guile 3.2, replace this union with just a "size" member.
     Digits are always allocated inline.  */
  union {
    mpz_t mpz;
    struct {
      int zero;
      int size;
      mp_limb_t *limbs;
    } z;
  } u;
  mp_limb_t limbs[];
};

static inline struct scm_bignum *
scm_bignum (SCM x)
{
  ASSERT (SCM_BIGP (x));
  return (struct scm_bignum *) SCM_UNPACK (x);
}

static int
bignum_size (struct scm_bignum *z)
{
  return z->u.z.size;
}

static int
bignum_is_negative (struct scm_bignum *z)
{
  return bignum_size (z) < 0;
}

static size_t
bignum_limb_count (struct scm_bignum *z)
{
  return bignum_is_negative (z) ? -bignum_size (z) : bignum_size (z);
}

static mp_limb_t*
bignum_limbs (struct scm_bignum *z)
{
  // FIXME: In the future we can just return z->limbs.
  return z->u.z.limbs;
}

static inline unsigned long
long_magnitude (long l)
{
  unsigned long mag = l;
  return l < 0 ? ~mag + 1 : mag;
}

static inline long
negative_long (unsigned long mag)
{
  ASSERT (mag <= (unsigned long) LONG_MIN);
  return ~mag + 1;
}

static inline scm_t_bits
inum_magnitude (scm_t_inum i)
{
  scm_t_bits mag = i;
  if (i < 0)
    mag = ~mag + 1;
  return mag;
}

static struct scm_bignum *
allocate_bignum (size_t nlimbs)
{
  ASSERT (nlimbs <= (size_t)INT_MAX);
  ASSERT (nlimbs <= NLIMBS_MAX);

  size_t size = sizeof (struct scm_bignum) + nlimbs * sizeof(mp_limb_t);
  struct scm_bignum *z = scm_gc_malloc_pointerless (size, "bignum");

  z->tag = scm_tc16_big;

  z->u.z.zero = 0;
  z->u.z.size = nlimbs;
  z->u.z.limbs = z->limbs;

  // _mp_alloc == 0 means GMP will never try to free this memory.
  ASSERT (z->u.mpz[0]._mp_alloc == 0);
  // Our "size" field should alias the mpz's _mp_size field.
  ASSERT (z->u.mpz[0]._mp_size == nlimbs);
  // Limbs are always allocated inline.
  ASSERT (z->u.mpz[0]._mp_d == z->limbs);

  // z->limbs left uninitialized.
  return z;
}

static struct scm_bignum *
negate_bignum (struct scm_bignum *z)
{
  z->u.z.size = -z->u.z.size;
  return z;
}

static SCM
make_bignum_1 (int is_negative, mp_limb_t limb)
{
  struct scm_bignum *z = allocate_bignum (1);
  z->limbs[0] = limb;
  return SCM_PACK (is_negative ? negate_bignum(z) : z);
}

static SCM
ulong_to_bignum (unsigned long u)
{
  ASSERT (!SCM_POSFIXABLE (u));
  return make_bignum_1 (0, u);
};

static SCM
long_to_bignum (long i)
{
  if (i > 0)
    return ulong_to_bignum (i);

  ASSERT (!SCM_NEGFIXABLE (i));
  return make_bignum_1 (1, long_magnitude (i));
};

static SCM
inum_to_bignum (scm_t_inum i)
{
  return long_to_bignum (i);
};

static struct scm_bignum *
clone_bignum (struct scm_bignum *z)
{
  struct scm_bignum *ret = allocate_bignum (bignum_limb_count (z));
  mpn_copyi (bignum_limbs (ret), bignum_limbs (z), bignum_limb_count (z));
  return bignum_is_negative (z) ? negate_bignum (ret) : ret;
}

static void
alias_bignum_to_mpz (struct scm_bignum *z, mpz_ptr mpz)
{
  // No need to clear this mpz.
  mpz->_mp_alloc = 0;
  mpz->_mp_size = bignum_size (z);
  // Gotta be careful to keep z alive.
  mpz->_mp_d = bignum_limbs (z);
}

static struct scm_bignum *
make_bignum_from_mpz (mpz_srcptr mpz)
{
  size_t nlimbs = mpz_size (mpz);
  struct scm_bignum *ret = allocate_bignum (nlimbs);
  mpn_copyi (bignum_limbs (ret), mpz_limbs_read (mpz), nlimbs);
  return mpz_sgn (mpz) < 0 ? negate_bignum (ret) : ret;
}

static SCM
normalize_bignum (struct scm_bignum *z)
{
  switch (bignum_size (z))
    {
    case -1:
      if (bignum_limbs (z)[0] <= inum_magnitude (SCM_MOST_NEGATIVE_FIXNUM))
        return SCM_I_MAKINUM (negative_long (bignum_limbs (z)[0]));
      break;
    case 0:
      return SCM_INUM0;
    case 1:
      if (bignum_limbs (z)[0] <= SCM_MOST_POSITIVE_FIXNUM)
        return SCM_I_MAKINUM (bignum_limbs (z)[0]);
      break;
    default:
      break;
    }
  return SCM_PACK (z);
}

int
scm_is_integer_odd_i (scm_t_inum i)
{
  return i & 1;
}

int
scm_is_integer_odd_z (SCM z)
{
  return bignum_limbs (scm_bignum (z))[0] & 1;
}
