/* Copyright 1995-2016,2018-2022
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

#include <math.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <verify.h>

#include "boolean.h"
#include "numbers.h"
#include "strings.h"

#include "integers.h"

/* Some functions that use GMP's mpn functions assume that a
   non-negative fixnum will always fit in a 'mp_limb_t'.  */
verify (SCM_MOST_POSITIVE_FIXNUM <= (mp_limb_t) -1);

#define NLIMBS_MAX (SSIZE_MAX / sizeof(mp_limb_t))

#if !(__MINGW32__ && __x86_64__)
#define L1 1L
#else /* (__MINGW32__ && __x86_64__) */
#define L1 1LL
#endif /* (__MINGW32__ && __x86_64__) */

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

static int
bignum_is_positive (struct scm_bignum *z)
{
  return bignum_size (z) > 0;
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

static inline uintptr_t
intptr_t_magnitude (intptr_t l)
{
  uintptr_t mag = l;
  return l < 0 ? ~mag + 1 : mag;
}

static inline intptr_t
negative_intptr_t (uintptr_t mag)
{
  ASSERT (mag <= (uintptr_t) INTPTR_MIN);
  return ~mag + 1;
}

static inline int64_t
negative_int64 (uint64_t mag)
{
  ASSERT (mag <= (uint64_t) INT64_MIN);
  return ~mag + 1;
}

static inline uint64_t
int64_magnitude (int64_t i)
{
  uint64_t mag = i;
  if (i < 0)
    mag = ~mag + 1;
  return mag;
}

static inline scm_t_bits
inum_magnitude (intptr_t i)
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
bignum_trim1 (struct scm_bignum *z)
{
  ASSERT (z->u.z.size > 0);
  z->u.z.size -= (z->limbs[z->u.z.size - 1] == 0);
  return z;
}

static struct scm_bignum *
bignum_trimn (struct scm_bignum *z)
{
  ASSERT (z->u.z.size > 0);
  while (z->u.z.size > 0 && z->limbs[z->u.z.size - 1] == 0)
    z->u.z.size--;
  return z;
}

static struct scm_bignum *
negate_bignum (struct scm_bignum *z)
{
  z->u.z.size = -z->u.z.size;
  return z;
}

static struct scm_bignum *
bignum_negate_if (int negate, struct scm_bignum *z)
{
  return negate ? negate_bignum (z) : z;
}

static struct scm_bignum *
make_bignum_0 (void)
{
  return allocate_bignum (0);
}

static struct scm_bignum *
make_bignum_1 (int is_negative, mp_limb_t limb)
{
  struct scm_bignum *z = allocate_bignum (1);
  z->limbs[0] = limb;
  return is_negative ? negate_bignum(z) : z;
}

static struct scm_bignum *
make_bignum_2 (int is_negative, mp_limb_t lo, mp_limb_t hi)
{
  struct scm_bignum *z = allocate_bignum (2);
  z->limbs[0] = lo;
  z->limbs[1] = hi;
  return is_negative ? negate_bignum(z) : z;
}

static struct scm_bignum *
make_bignum_from_uint64 (uint64_t val)
{
#if SCM_SIZEOF_INTPTR_T == 4
  if (val > UINT32_MAX)
    return make_bignum_2 (0, val, val >> 32);
#endif
  return val == 0 ? make_bignum_0 () : make_bignum_1 (0, val);
}

static struct scm_bignum *
make_bignum_from_int64 (int64_t val)
{
  return val < 0
    ? negate_bignum (make_bignum_from_uint64 (int64_magnitude (val)))
    : make_bignum_from_uint64 (val);
}

static struct scm_bignum *
uintptr_t_to_bignum (uintptr_t u)
{
  return u == 0 ? make_bignum_0 () : make_bignum_1 (0, u);
};

static struct scm_bignum *
intptr_t_to_bignum (intptr_t i)
{
  if (i > 0)
    return uintptr_t_to_bignum (i);

  return i == 0 ? make_bignum_0 () : make_bignum_1 (1, intptr_t_magnitude (i));
};

static inline SCM
scm_from_bignum (struct scm_bignum *x)
{
  return SCM_PACK (x);
}

static SCM
intptr_t_to_scm (intptr_t i)
{
  if (SCM_FIXABLE (i))
    return SCM_I_MAKINUM (i);
  return scm_from_bignum (intptr_t_to_bignum (i));
}

static SCM
uintptr_t_to_scm (uintptr_t i)
{
  if (SCM_POSFIXABLE (i))
    return SCM_I_MAKINUM (i);
  return scm_from_bignum (uintptr_t_to_bignum (i));
}

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
        return SCM_I_MAKINUM (negative_intptr_t (bignum_limbs (z)[0]));
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
  return scm_from_bignum (z);
}

static SCM
take_mpz (mpz_ptr mpz)
{
  SCM ret;
  if (mpz_fits_slong_p (mpz))
    ret = intptr_t_to_scm (mpz_get_si (mpz));
  else
    ret = scm_from_bignum (make_bignum_from_mpz (mpz));
  mpz_clear (mpz);
  return ret;
}

static int
intptr_t_sign (intptr_t l)
{
  if (l < 0) return -1;
  if (l == 0) return 0;
  return 1;
}

static int
negative_uint64_to_int64 (uint64_t magnitude, int64_t *val)
{
  if (magnitude > int64_magnitude (INT64_MIN))
    return 0;
  *val = negative_int64 (magnitude);
  return 1;
}

static int
positive_uint64_to_int64 (uint64_t magnitude, int64_t *val)
{
  if (magnitude > INT64_MAX)
    return 0;
  *val = magnitude;
  return 1;
}

static int
bignum_to_int64 (struct scm_bignum *z, int64_t *val)
{
  switch (bignum_size (z))
    {
#if SCM_SIZEOF_INTPTR_T == 4
    case -2:
      {
        uint64_t mag = bignum_limbs (z)[0];
        mag |= ((uint64_t) bignum_limbs (z)[1]) << 32;
        return negative_uint64_to_int64 (mag, val);
      }
#endif
    case -1:
      return negative_uint64_to_int64 (bignum_limbs (z)[0], val);
    case 0:
      *val = 0;
      return 1;
    case 1:
      return positive_uint64_to_int64 (bignum_limbs (z)[0], val);
#if SCM_SIZEOF_INTPTR_T == 4
    case 2:
      {
        uint64_t mag = bignum_limbs (z)[0];
        mag |= ((uint64_t) bignum_limbs (z)[1]) << 32;
        return positive_uint64_to_int64 (mag, val);
      }
#endif
    default:
      return 0;
    }
}

static int
bignum_to_uint64 (struct scm_bignum *z, uint64_t *val)
{
  switch (bignum_size (z))
    {
    case 0:
      *val = 0;
      return 1;
    case 1:
      *val = bignum_limbs (z)[0];
      return 1;
#if SCM_SIZEOF_INTPTR_T == 4
    case 2:
      {
        uint64_t mag = bignum_limbs (z)[0];
        mag |= ((uint64_t) bignum_limbs (z)[1]) << 32;
        *val = mag;
        return 1;
      }
#endif
    default:
      return 0;
    }
}

#if SCM_SIZEOF_INTPTR_T == 4
static int
negative_uint32_to_int32 (uint32_t magnitude, int32_t *val)
{
  if (magnitude > intptr_t_magnitude (INT32_MIN))
    return 0;
  *val = negative_intptr_t (magnitude);
  return 1;
}

static int
positive_uint32_to_int32 (uint32_t magnitude, int32_t *val)
{
  if (magnitude > INT32_MAX)
    return 0;
  *val = magnitude;
  return 1;
}

static int
bignum_to_int32 (struct scm_bignum *z, int32_t *val)
{
  switch (bignum_size (z))
    {
    case -1:
      return negative_uint32_to_int32 (bignum_limbs (z)[0], val);
    case 0:
      *val = 0;
      return 1;
    case 1:
      return positive_uint32_to_int32 (bignum_limbs (z)[0], val);
    default:
      return 0;
    }
}

static int
bignum_to_uint32 (struct scm_bignum *z, uint32_t *val)
{
  switch (bignum_size (z))
    {
    case 0:
      *val = 0;
      return 1;
    case 1:
      *val = bignum_limbs (z)[0];
      return 1;
    default:
      return 0;
    }
}
#endif

static int
bignum_cmp_intptr_t (struct scm_bignum *z, intptr_t l)
{
  switch (bignum_size (z))
    {
    case -1:
      if (l >= 0)
        return -1;
      return intptr_t_sign (intptr_t_magnitude (l) - bignum_limbs (z)[0]);
    case 0:
      return intptr_t_sign (l);
    case 1:
      if (l <= 0)
        return 1;
      return intptr_t_sign (bignum_limbs (z)[0] - (uintptr_t) l);
    default:
      return intptr_t_sign (bignum_size (z));
    }
}

SCM
scm_integer_from_mpz (const mpz_t mpz)
{
  return normalize_bignum (make_bignum_from_mpz (mpz));
}

int
scm_is_integer_odd_i (intptr_t i)
{
  return i & 1;
}

int
scm_is_integer_odd_z (struct scm_bignum *z)
{
  return bignum_limbs (z)[0] & 1;
}

SCM
scm_integer_abs_i (intptr_t i)
{
  if (i >= 0)
    return SCM_I_MAKINUM (i);

  return uintptr_t_to_scm (intptr_t_magnitude (i));
}

SCM
scm_integer_abs_z (struct scm_bignum *z)
{
  if (!bignum_is_negative (z))
    return scm_from_bignum (z);

  return scm_integer_negate_z (z);
}

SCM
scm_integer_floor_quotient_ii (intptr_t x, intptr_t y)
{
  if (y > 0)
    {
      if (x < 0)
        x = x - y + 1;
    }
  else if (y == 0)
    scm_num_overflow ("floor-quotient");
  else if (x > 0)
    x = x - y - 1;
  intptr_t q = x / y;
  return intptr_t_to_scm (q);
}

SCM
scm_integer_floor_quotient_iz (intptr_t x, struct scm_bignum *y)
{
  if (x == 0 || ((x < 0) == bignum_is_negative (y)))
    return SCM_INUM0;
  return SCM_I_MAKINUM (-1);
}
 
SCM
scm_integer_floor_quotient_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("floor-quotient");
  else if (y == 1)
    return scm_from_bignum (x);

  mpz_t zx, q;
  alias_bignum_to_mpz (x, zx);
  mpz_init (q);
  if (y > 0)
    mpz_fdiv_q_ui (q, zx, y);
  else
    {
      mpz_cdiv_q_ui (q, zx, -y);
      mpz_neg (q, q);
    }
  scm_remember_upto_here_1 (x);
  return take_mpz (q);
}

SCM
scm_integer_floor_quotient_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t zx, zy, q;
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_init (q);
  mpz_fdiv_q (q, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (q);
}

SCM
scm_integer_floor_remainder_ii (intptr_t x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("floor-remainder");
  intptr_t r = x % y;
  int needs_adjustment = (y > 0) ? (r < 0) : (r > 0);
  if (needs_adjustment)
    r += y;
  return SCM_I_MAKINUM (r);
}

SCM
scm_integer_floor_remainder_iz (intptr_t x, struct scm_bignum *y)
{
  if (bignum_is_positive (y))
    {
      if (x < 0)
        {
          mpz_t r, zy;
          mpz_init (r);
          alias_bignum_to_mpz (y, zy);
          mpz_sub_ui (r, zy, -x);
          scm_remember_upto_here_1 (y);
          return take_mpz (r);
        }
      else
        return SCM_I_MAKINUM (x);
    }
  else if (x <= 0)
    return SCM_I_MAKINUM (x);
  else
    {
      mpz_t r, zy;
      mpz_init (r);
      alias_bignum_to_mpz (y, zy);
      mpz_add_ui (r, zy, x);
      scm_remember_upto_here_1 (y);
      return take_mpz (r);
    }
}

SCM
scm_integer_floor_remainder_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("floor-remainder");
  else
    {
      intptr_t r;
      mpz_t zx;
      alias_bignum_to_mpz (x, zx);
      if (y > 0)
        r = mpz_fdiv_ui (zx, y);
      else
        r = -mpz_cdiv_ui (zx, -y);
      scm_remember_upto_here_1 (x);
      return SCM_I_MAKINUM (r);
    }
}

SCM
scm_integer_floor_remainder_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t zx, zy, r;
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_init (r);
  mpz_fdiv_r (r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (r);
}

void
scm_integer_floor_divide_ii (intptr_t x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("floor-divide");

  intptr_t q = x / y;
  intptr_t r = x % y;
  int needs_adjustment = (y > 0) ? (r < 0) : (r > 0);

  if (needs_adjustment)
    {
      r += y;
      q--;
    }

  *qp = intptr_t_to_scm (q);
  *rp = SCM_I_MAKINUM (r);
}

void
scm_integer_floor_divide_iz (intptr_t x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  if (bignum_is_positive (y))
    {
      if (x < 0)
        {
          mpz_t zy, r;
          alias_bignum_to_mpz (y, zy);
          mpz_init (r);
          mpz_sub_ui (r, zy, -x);
          scm_remember_upto_here_1 (y);
          *qp = SCM_I_MAKINUM (-1);
          *rp = take_mpz (r);
        }
      else
        {
          *qp = SCM_INUM0;
          *rp = SCM_I_MAKINUM (x);
        }
    }
  else if (x <= 0)
    {
      *qp = SCM_INUM0;
      *rp = SCM_I_MAKINUM (x);
    }
  else
    {
      mpz_t zy, r;
      alias_bignum_to_mpz (y, zy);
      mpz_init (r);
      mpz_add_ui (r, zy, x);
      scm_remember_upto_here_1 (y);
      *qp = SCM_I_MAKINUM (-1);
      *rp = take_mpz (r);
    }
}

void
scm_integer_floor_divide_zi (struct scm_bignum *x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("floor-divide");

  mpz_t zx, q, r;
  alias_bignum_to_mpz (x, zx);
  mpz_init (q);
  mpz_init (r);
  if (y > 0)
    mpz_fdiv_qr_ui (q, r, zx, y);
  else
    {
      mpz_cdiv_qr_ui (q, r, zx, -y);
      mpz_neg (q, q);
    }
  scm_remember_upto_here_1 (x);
  *qp = take_mpz (q);
  *rp = take_mpz (r);
}

void
scm_integer_floor_divide_zz (struct scm_bignum *x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  mpz_t zx, zy, q, r;
  mpz_init (q);
  mpz_init (r);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_fdiv_qr (q, r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  *qp = take_mpz (q);
  *rp = take_mpz (r);
}

SCM
scm_integer_ceiling_quotient_ii (intptr_t x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("ceiling-quotient");

  if (y > 0)
    {
      if (x >= 0)
        x = x + y - 1;
    }
  else if (x < 0)
    x = x + y + 1;
  intptr_t q = x / y;

  return intptr_t_to_scm (q);
}

SCM
scm_integer_ceiling_quotient_iz (intptr_t x, struct scm_bignum *y)
{
  if (bignum_is_positive (y))
    {
      if (x > 0)
        return SCM_INUM1;
      else if (x == SCM_MOST_NEGATIVE_FIXNUM &&
               bignum_cmp_intptr_t (y, -SCM_MOST_NEGATIVE_FIXNUM) == 0)
        {
          /* Special case: x == fixnum-min && y == abs (fixnum-min) */
          scm_remember_upto_here_1 (y);
          return SCM_I_MAKINUM (-1);
        }
      else
        return SCM_INUM0;
    }
  else if (x >= 0)
    return SCM_INUM0;
  else
    return SCM_INUM1;
}

SCM
scm_integer_ceiling_quotient_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("ceiling-quotient");
  else if (y == 1)
    return scm_from_bignum (x);
  else
    {
      mpz_t q, zx;
      mpz_init (q);
      alias_bignum_to_mpz (x, zx);
      if (y > 0)
        mpz_cdiv_q_ui (q, zx, y);
      else
        {
          mpz_fdiv_q_ui (q, zx, -y);
          mpz_neg (q, q);
        }
      scm_remember_upto_here_1 (x);
      return take_mpz (q);
    }
}

SCM
scm_integer_ceiling_quotient_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t q, zx, zy;
  mpz_init (q);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_cdiv_q (q, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (q);
}

SCM
scm_integer_ceiling_remainder_ii (intptr_t x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("ceiling-remainder");

  intptr_t r = x % y;
  int needs_adjustment = (y > 0) ? (r > 0) : (r < 0);
  if (needs_adjustment)
    r -= y;

  return SCM_I_MAKINUM (r);
}

SCM
scm_integer_ceiling_remainder_iz (intptr_t x, struct scm_bignum *y)
{
  if (bignum_is_positive (y))
    {
      if (x > 0)
        {
          mpz_t r, zy;
          mpz_init (r);
          alias_bignum_to_mpz (y, zy);
          mpz_sub_ui (r, zy, x);
          scm_remember_upto_here_1 (y);
          mpz_neg (r, r);
          return take_mpz (r);
        }
      else if (x == SCM_MOST_NEGATIVE_FIXNUM &&
               bignum_cmp_intptr_t (y, -SCM_MOST_NEGATIVE_FIXNUM) == 0)
        {
          /* Special case: x == fixnum-min && y == abs (fixnum-min) */
          scm_remember_upto_here_1 (y);
          return SCM_INUM0;
        }
      else
        return SCM_I_MAKINUM (x);
    }
  else if (x >= 0)
    return SCM_I_MAKINUM (x);
  else
    {
      mpz_t r, zy;
      mpz_init (r);
      alias_bignum_to_mpz (y, zy);
      mpz_add_ui (r, zy, -x);
      scm_remember_upto_here_1 (y);
      mpz_neg (r, r);
      return take_mpz (r);
    }
}

SCM
scm_integer_ceiling_remainder_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("ceiling-remainder");
  else
    {
      mpz_t zx;
      alias_bignum_to_mpz (x, zx);
      intptr_t r;
      if (y > 0)
        r = -mpz_cdiv_ui (zx, y);
      else
        r = mpz_fdiv_ui (zx, -y);
      scm_remember_upto_here_1 (x);
      return SCM_I_MAKINUM (r);
    }
}

SCM
scm_integer_ceiling_remainder_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t r, zx, zy;
  mpz_init (r);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_cdiv_r (r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (r);
}

void
scm_integer_ceiling_divide_ii (intptr_t x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("ceiling-divide");
  else
    {
      intptr_t q = x / y;
      intptr_t r = x % y;
      int needs_adjustment;

      if (y > 0)
        needs_adjustment = (r > 0);
      else
        needs_adjustment = (r < 0);

      if (needs_adjustment)
        {
          r -= y;
          q++;
        }
      *qp = intptr_t_to_scm (q);
      *rp = SCM_I_MAKINUM (r);
    }
}

void
scm_integer_ceiling_divide_iz (intptr_t x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  if (bignum_is_positive (y))
    {
      if (x > 0)
        {
          mpz_t r, zy;
          mpz_init (r);
          alias_bignum_to_mpz (y, zy);
          mpz_sub_ui (r, zy, x);
          scm_remember_upto_here_1 (y);
          mpz_neg (r, r);
          *qp = SCM_INUM1;
          *rp = take_mpz (r);
        }
      else if (x == SCM_MOST_NEGATIVE_FIXNUM &&
               bignum_cmp_intptr_t (y, -SCM_MOST_NEGATIVE_FIXNUM) == 0)
        {
          /* Special case: x == fixnum-min && y == abs (fixnum-min) */
          scm_remember_upto_here_1 (y);
          *qp = SCM_I_MAKINUM (-1);
          *rp = SCM_INUM0;
        }
      else
        {
          *qp = SCM_INUM0;
          *rp = SCM_I_MAKINUM (x);
        }
    }
  else if (x >= 0)
    {
      *qp = SCM_INUM0;
      *rp = SCM_I_MAKINUM (x);
    }
  else
    {
      mpz_t r, zy;
      mpz_init (r);
      alias_bignum_to_mpz (y, zy);
      mpz_add_ui (r, zy, -x);
      scm_remember_upto_here_1 (y);
      mpz_neg (r, r);
      *qp = SCM_INUM1;
      *rp = take_mpz (r);
    }
}

void
scm_integer_ceiling_divide_zi (struct scm_bignum *x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("ceiling-divide");
  else
    {
      mpz_t q, r, zx;
      mpz_init (q);
      mpz_init (r);
      alias_bignum_to_mpz (x, zx);
      if (y > 0)
        mpz_cdiv_qr_ui (q, r, zx, y);
      else
        {
          mpz_fdiv_qr_ui (q, r, zx, -y);
          mpz_neg (q, q);
        }
      scm_remember_upto_here_1 (x);
      *qp = take_mpz (q);
      *rp = take_mpz (r);
    }
}

void
scm_integer_ceiling_divide_zz (struct scm_bignum *x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  mpz_t q, r, zx, zy;
  mpz_init (q);
  mpz_init (r);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_cdiv_qr (q, r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  *qp = take_mpz (q);
  *rp = take_mpz (r);
}

SCM
scm_integer_truncate_quotient_ii (intptr_t x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("truncate-quotient");
  else
    {
      intptr_t q = x / y;
      return intptr_t_to_scm (q);
    }
}

SCM
scm_integer_truncate_quotient_iz (intptr_t x, struct scm_bignum *y)
{
  if (x == SCM_MOST_NEGATIVE_FIXNUM &&
      bignum_cmp_intptr_t (y, -SCM_MOST_NEGATIVE_FIXNUM) == 0)
    {
      /* Special case: x == fixnum-min && y == abs (fixnum-min) */
      scm_remember_upto_here_1 (y);
      return SCM_I_MAKINUM (-1);
    }
  else
    return SCM_INUM0;
}

SCM
scm_integer_truncate_quotient_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("truncate-quotient");
  else if (y == 1)
    return scm_from_bignum (x);
  else
    {
      mpz_t q, zx;
      mpz_init (q);
      alias_bignum_to_mpz (x, zx);
      if (y > 0)
        mpz_tdiv_q_ui (q, zx, y);
      else
        {
          mpz_tdiv_q_ui (q, zx, -y);
          mpz_neg (q, q);
        }
      scm_remember_upto_here_1 (x);
      return take_mpz (q);
    }
}

SCM
scm_integer_truncate_quotient_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t q, zx, zy;
  mpz_init (q);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_tdiv_q (q, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (q);
}

SCM
scm_integer_truncate_remainder_ii (intptr_t x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("truncate-remainder");
  else
    {
      intptr_t q = x % y;
      return intptr_t_to_scm (q);
    }
}

SCM
scm_integer_truncate_remainder_iz (intptr_t x, struct scm_bignum *y)
{
  if (x == SCM_MOST_NEGATIVE_FIXNUM &&
      bignum_cmp_intptr_t (y, -SCM_MOST_NEGATIVE_FIXNUM) == 0)
    {
      /* Special case: x == fixnum-min && y == abs (fixnum-min) */
      scm_remember_upto_here_1 (y);
      return SCM_INUM0;
    }
  else
    return SCM_I_MAKINUM (x);
}

SCM
scm_integer_truncate_remainder_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("truncate-remainder");
  else
    {
      mpz_t zx;
      alias_bignum_to_mpz (x, zx);
      intptr_t r = mpz_tdiv_ui (zx, (y > 0) ? y : -y) * mpz_sgn (zx);
      scm_remember_upto_here_1 (x);
      return SCM_I_MAKINUM (r);
    }
}

SCM
scm_integer_truncate_remainder_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t r, zx, zy;
  mpz_init (r);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_tdiv_r (r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (r);
}

void
scm_integer_truncate_divide_ii (intptr_t x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("truncate-divide");
  else
    {
      intptr_t q = x / y;
      intptr_t r = x % y;
      *qp = intptr_t_to_scm (q);
      *rp = SCM_I_MAKINUM (r);
    }
}

void
scm_integer_truncate_divide_iz (intptr_t x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  if (x == SCM_MOST_NEGATIVE_FIXNUM &&
      bignum_cmp_intptr_t (y, -SCM_MOST_NEGATIVE_FIXNUM) == 0)
    {
      /* Special case: x == fixnum-min && y == abs (fixnum-min) */
      scm_remember_upto_here_1 (y);
      *qp = SCM_I_MAKINUM (-1);
      *rp = SCM_INUM0;
    }
  else
    {
      *qp = SCM_INUM0;
      *rp = SCM_I_MAKINUM (x);
    }
}

void
scm_integer_truncate_divide_zi (struct scm_bignum *x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("truncate-divide");
  else
    {
      mpz_t q, zx;
      mpz_init (q);
      alias_bignum_to_mpz (x, zx);
      intptr_t r;
      if (y > 0)
        r = mpz_tdiv_q_ui (q, zx, y);
      else
        {
          r = mpz_tdiv_q_ui (q, zx, -y);
          mpz_neg (q, q);
        }
      r *= mpz_sgn (zx);
      scm_remember_upto_here_1 (x);
      *qp = take_mpz (q);
      *rp = SCM_I_MAKINUM (r);
    }
}

void
scm_integer_truncate_divide_zz (struct scm_bignum *x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  mpz_t q, r, zx, zy;
  mpz_init (q);
  mpz_init (r);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_tdiv_qr (q, r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  *qp = take_mpz (q);
  *rp = take_mpz (r);
}

static SCM
integer_centered_quotient_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t q, r, min_r, zx, zy;
  mpz_init (q);
  mpz_init (r);
  mpz_init (min_r);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);

  /* Note that x might be small enough to fit into a fixnum, so we must
     not let it escape into the wild.  */

  /* min_r will eventually become -abs(y)/2 */
  mpz_tdiv_q_2exp (min_r, zy, 1);

  /* Arrange for rr to initially be non-positive, because that
     simplifies the test to see if it is within the needed bounds. */
  if (mpz_sgn (zy) > 0)
    {
      mpz_cdiv_qr (q, r, zx, zy);
      scm_remember_upto_here_2 (x, y);
      mpz_neg (min_r, min_r);
      if (mpz_cmp (r, min_r) < 0)
	mpz_sub_ui (q, q, 1);
    }
  else
    {
      mpz_fdiv_qr (q, r, zx, zy);
      scm_remember_upto_here_2 (x, y);
      if (mpz_cmp (r, min_r) < 0)
        mpz_add_ui (q, q, 1);
    }
  mpz_clear (r);
  mpz_clear (min_r);
  return take_mpz (q);
}

SCM
scm_integer_centered_quotient_ii (intptr_t x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("centered-quotient");

  intptr_t q = x / y;
  intptr_t r = x % y;
  if (x > 0)
    {
      if (y > 0)
        {
          if (r >= (y + 1) / 2)
            q++;
        }
      else
        {
          if (r >= (1 - y) / 2)
            q--;
        }
    }
  else
    {
      if (y > 0)
        {
          if (r < -y / 2)
            q--;
        }
      else
        {
          if (r < y / 2)
            q++;
        }
    }
  return intptr_t_to_scm (q);
}

SCM
scm_integer_centered_quotient_iz (intptr_t x, struct scm_bignum *y)
{
  return integer_centered_quotient_zz (intptr_t_to_bignum (x),
                                       y);
}

SCM
scm_integer_centered_quotient_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("centered-quotient");
  else if (y == 1)
    return scm_from_bignum (x);
  else
    {
      mpz_t q, zx;
      mpz_init (q);
      alias_bignum_to_mpz (x, zx);
      intptr_t r;
      /* Arrange for r to initially be non-positive, because that
         simplifies the test to see if it is within the needed
         bounds. */
      if (y > 0)
        {
          r = - mpz_cdiv_q_ui (q, zx, y);
          scm_remember_upto_here_1 (x);
          if (r < -y / 2)
            mpz_sub_ui (q, q, 1);
        }
      else
        {
          r = - mpz_cdiv_q_ui (q, zx, -y);
          scm_remember_upto_here_1 (x);
          mpz_neg (q, q);
          if (r < y / 2)
            mpz_add_ui (q, q, 1);
        }
      return take_mpz (q);
    }
}

SCM
scm_integer_centered_quotient_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  return integer_centered_quotient_zz (x, y);
}

static SCM
integer_centered_remainder_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t r, min_r, zx, zy;
  mpz_init (r);
  mpz_init (min_r);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */

  /* min_r will eventually become -abs(y)/2 */
  mpz_tdiv_q_2exp (min_r, zy, 1);

  /* Arrange for r to initially be non-positive, because that simplifies
     the test to see if it is within the needed bounds. */
  if (mpz_sgn (zy) > 0)
    {
      mpz_cdiv_r (r, zx, zy);
      mpz_neg (min_r, min_r);
      if (mpz_cmp (r, min_r) < 0)
	mpz_add (r, r, zy);
    }
  else
    {
      mpz_fdiv_r (r, zx, zy);
      if (mpz_cmp (r, min_r) < 0)
	mpz_sub (r, r, zy);
    }
  scm_remember_upto_here_2 (x, y);
  mpz_clear (min_r);
  return take_mpz (r);
}

SCM
scm_integer_centered_remainder_ii (intptr_t x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("centered-remainder");

  intptr_t r = x % y;
  if (x > 0)
    {
      if (y > 0)
        {
          if (r >= (y + 1) / 2)
            r -= y;
        }
      else
        {
          if (r >= (1 - y) / 2)
            r += y;
        }
    }
  else
    {
      if (y > 0)
        {
          if (r < -y / 2)
            r += y;
        }
      else
        {
          if (r < y / 2)
            r -= y;
        }
    }
  return SCM_I_MAKINUM (r);
}

SCM
scm_integer_centered_remainder_iz (intptr_t x, struct scm_bignum *y)
{
  return integer_centered_remainder_zz (intptr_t_to_bignum (x),
                                        y);
}

SCM
scm_integer_centered_remainder_zi (struct scm_bignum *x, intptr_t y)
{
  mpz_t zx;
  alias_bignum_to_mpz (x, zx);

  if (y == 0)
    scm_num_overflow ("centered-remainder");

  intptr_t r;
  /* Arrange for r to initially be non-positive, because that simplifies
     the test to see if it is within the needed bounds. */
  if (y > 0)
    {
      r = - mpz_cdiv_ui (zx, y);
      if (r < -y / 2)
        r += y;
    }
  else
    {
      r = - mpz_cdiv_ui (zx, -y);
      if (r < y / 2)
        r -= y;
    }
  scm_remember_upto_here_1 (x);
  return SCM_I_MAKINUM (r);
}

SCM
scm_integer_centered_remainder_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  return integer_centered_remainder_zz (x, y);
}

static void
integer_centered_divide_zz (struct scm_bignum *x, struct scm_bignum *y,
                            SCM *qp, SCM *rp)
{
  mpz_t q, r, min_r, zx, zy;
  mpz_init (q);
  mpz_init (r);
  mpz_init (min_r);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);

  /* Note that x might be small enough to fit into a fixnum, so we must
     not let it escape into the wild */

  /* min_r will eventually become -abs(y/2) */
  mpz_tdiv_q_2exp (min_r, zy, 1);

  /* Arrange for rr to initially be non-positive, because that
     simplifies the test to see if it is within the needed bounds. */
  if (mpz_sgn (zy) > 0)
    {
      mpz_cdiv_qr (q, r, zx, zy);
      mpz_neg (min_r, min_r);
      if (mpz_cmp (r, min_r) < 0)
	{
	  mpz_sub_ui (q, q, 1);
	  mpz_add (r, r, zy);
	}
    }
  else
    {
      mpz_fdiv_qr (q, r, zx, zy);
      if (mpz_cmp (r, min_r) < 0)
	{
	  mpz_add_ui (q, q, 1);
	  mpz_sub (r, r, zy);
	}
    }
  scm_remember_upto_here_2 (x, y);
  mpz_clear (min_r);
  *qp = take_mpz (q);
  *rp = take_mpz (r);
}

void
scm_integer_centered_divide_ii (intptr_t x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("centered-divide");

  intptr_t q = x / y;
  intptr_t r = x % y;
  if (x > 0)
    {
      if (y > 0)
        {
          if (r >= (y + 1) / 2)
            { q++; r -= y; }
        }
      else
        {
          if (r >= (1 - y) / 2)
            { q--; r += y; }
        }
    }
  else
    {
      if (y > 0)
        {
          if (r < -y / 2)
            { q--; r += y; }
        }
      else
        {
          if (r < y / 2)
            { q++; r -= y; }
        }
    }
  *qp = intptr_t_to_scm (q);
  *rp = SCM_I_MAKINUM (r);
}

void
scm_integer_centered_divide_iz (intptr_t x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  integer_centered_divide_zz (intptr_t_to_bignum (x), y, qp, rp);
}

void
scm_integer_centered_divide_zi (struct scm_bignum *x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("centered-divide");

  mpz_t q, zx;
  mpz_init (q);
  alias_bignum_to_mpz (x, zx);
  intptr_t r;

  /* Arrange for r to initially be non-positive, because that
     simplifies the test to see if it is within the needed bounds. */

  if (y > 0)
    {
      r = - mpz_cdiv_q_ui (q, zx, y);
      if (r < -y / 2)
        {
          mpz_sub_ui (q, q, 1);
          r += y;
        }
    }
  else
    {
      r = - mpz_cdiv_q_ui (q, zx, -y);
      mpz_neg (q, q);
      if (r < y / 2)
        {
          mpz_add_ui (q, q, 1);
          r -= y;
        }
    }
  scm_remember_upto_here_1 (x);
  *qp = take_mpz (q);
  *rp = SCM_I_MAKINUM (r);
}

void
scm_integer_centered_divide_zz (struct scm_bignum *x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  integer_centered_divide_zz (x, y, qp, rp);
}

static SCM
integer_round_quotient_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t q, r, r2, zx, zy;
  int cmp, needs_adjustment;

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */
  mpz_init (q);
  mpz_init (r);
  mpz_init (r2);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);

  mpz_fdiv_qr (q, r, zx, zy);
  mpz_mul_2exp (r2, r, 1);  /* r2 = 2*r */
  scm_remember_upto_here_1 (x);

  cmp = mpz_cmpabs (r2, zy);
  if (mpz_odd_p (q))
    needs_adjustment = (cmp >= 0);
  else
    needs_adjustment = (cmp > 0);
  scm_remember_upto_here_1 (y);

  if (needs_adjustment)
    mpz_add_ui (q, q, 1);

  mpz_clear (r);
  mpz_clear (r2);
  return take_mpz (q);
}

SCM
scm_integer_round_quotient_ii (intptr_t x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("round-quotient");

  intptr_t q = x / y;
  intptr_t r = x % y;
  intptr_t ay = y;
  intptr_t r2 = 2 * r;

  if (y < 0)
    {
      ay = -ay;
      r2 = -r2;
    }

  if (q & L1)
    {
      if (r2 >= ay)
        q++;
      else if (r2 <= -ay)
        q--;
    }
  else
    {
      if (r2 > ay)
        q++;
      else if (r2 < -ay)
        q--;
    }
  return intptr_t_to_scm (q);
}

SCM
scm_integer_round_quotient_iz (intptr_t x, struct scm_bignum *y)
{
  return integer_round_quotient_zz (intptr_t_to_bignum (x), y);
}

SCM
scm_integer_round_quotient_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("round-quotient");
  if (y == 1)
    return scm_from_bignum (x);

  mpz_t q, zx;
  mpz_init (q);
  alias_bignum_to_mpz (x, zx);
  intptr_t r;
  int needs_adjustment;

  if (y > 0)
    {
      r = mpz_fdiv_q_ui (q, zx, y);
      if (mpz_odd_p (q))
        needs_adjustment = (2*r >= y);
      else
        needs_adjustment = (2*r > y);
    }
  else
    {
      r = - mpz_cdiv_q_ui (q, zx, -y);
      mpz_neg (q, q);
      if (mpz_odd_p (q))
        needs_adjustment = (2*r <= y);
      else
        needs_adjustment = (2*r < y);
    }
  scm_remember_upto_here_1 (x);
  if (needs_adjustment)
    mpz_add_ui (q, q, 1);
  return take_mpz (q);
}

SCM
scm_integer_round_quotient_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t q, r, zx, zy;
  int cmp, needs_adjustment;

  mpz_init (q);
  mpz_init (r);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);

  mpz_fdiv_qr (q, r, zx, zy);
  scm_remember_upto_here_1 (x);
  mpz_mul_2exp (r, r, 1);  /* r = 2*r */

  cmp = mpz_cmpabs (r, zy);
  mpz_clear (r);
  scm_remember_upto_here_1 (y);
  if (mpz_odd_p (q))
    needs_adjustment = (cmp >= 0);
  else
    needs_adjustment = (cmp > 0);

  if (needs_adjustment)
    mpz_add_ui (q, q, 1);

  return take_mpz (q);
}

static SCM
integer_round_remainder_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t q, r, r2, zx, zy;
  int cmp, needs_adjustment;

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */
  mpz_init (q);
  mpz_init (r);
  mpz_init (r2);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);

  mpz_fdiv_qr (q, r, zx, zy);
  scm_remember_upto_here_1 (x);
  mpz_mul_2exp (r2, r, 1);  /* r2 = 2*r */

  cmp = mpz_cmpabs (r2, zy);
  if (mpz_odd_p (q))
    needs_adjustment = (cmp >= 0);
  else
    needs_adjustment = (cmp > 0);

  if (needs_adjustment)
    mpz_sub (r, r, zy);

  scm_remember_upto_here_1 (y);
  mpz_clear (q);
  mpz_clear (r2);
  return take_mpz (r);
}

SCM
scm_integer_round_remainder_ii (intptr_t x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("round-remainder");

  intptr_t q = x / y;
  intptr_t r = x % y;
  intptr_t ay = y;
  intptr_t r2 = 2 * r;

  if (y < 0)
    {
      ay = -ay;
      r2 = -r2;
    }

  if (q & L1)
    {
      if (r2 >= ay)
        r -= y;
      else if (r2 <= -ay)
        r += y;
    }
  else
    {
      if (r2 > ay)
        r -= y;
      else if (r2 < -ay)
        r += y;
    }

  return SCM_I_MAKINUM (r);
}

SCM
scm_integer_round_remainder_iz (intptr_t x, struct scm_bignum *y)
{
  return integer_round_remainder_zz (intptr_t_to_bignum (x), y);
}

SCM
scm_integer_round_remainder_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    scm_num_overflow ("round-remainder");

  mpz_t q, zx;
  intptr_t r;
  int needs_adjustment;

  mpz_init (q);
  alias_bignum_to_mpz (x, zx);

  if (y > 0)
    {
      r = mpz_fdiv_q_ui (q, zx, y);
      if (mpz_odd_p (q))
        needs_adjustment = (2*r >= y);
      else
        needs_adjustment = (2*r > y);
    }
  else
    {
      r = - mpz_cdiv_q_ui (q, zx, -y);
      if (mpz_odd_p (q))
        needs_adjustment = (2*r <= y);
      else
        needs_adjustment = (2*r < y);
    }
  scm_remember_upto_here_1 (x);
  mpz_clear (q);
  if (needs_adjustment)
    r -= y;
  return SCM_I_MAKINUM (r);
}

SCM
scm_integer_round_remainder_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  return integer_round_remainder_zz (x, y);
}

static void
integer_round_divide_zz (struct scm_bignum *x, struct scm_bignum *y,
                         SCM *qp, SCM *rp)
{
  mpz_t q, r, r2, zx, zy;
  int cmp, needs_adjustment;

  /* Note that x might be small enough to fit into a fixnum, so we must
     not let it escape into the wild */
  mpz_init (q);
  mpz_init (r);
  mpz_init (r2);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);

  mpz_fdiv_qr (q, r, zx, zy);
  scm_remember_upto_here_1 (x);
  mpz_mul_2exp (r2, r, 1);  /* r2 = 2*r */

  cmp = mpz_cmpabs (r2, zy);
  if (mpz_odd_p (q))
    needs_adjustment = (cmp >= 0);
  else
    needs_adjustment = (cmp > 0);

  if (needs_adjustment)
    {
      mpz_add_ui (q, q, 1);
      mpz_sub (r, r, zy);
    }

  scm_remember_upto_here_1 (y);
  mpz_clear (r2);
  *qp = take_mpz (q);
  *rp = take_mpz (r);
}

void
scm_integer_round_divide_ii (intptr_t x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("round-divide");

  intptr_t q = x / y;
  intptr_t r = x % y;
  intptr_t ay = y;
  intptr_t r2 = 2 * r;

  if (y < 0)
    {
      ay = -ay;
      r2 = -r2;
    }

  if (q & L1)
    {
      if (r2 >= ay)
        { q++; r -= y; }
      else if (r2 <= -ay)
        { q--; r += y; }
    }
  else
    {
      if (r2 > ay)
        { q++; r -= y; }
      else if (r2 < -ay)
        { q--; r += y; }
    }
  *qp = intptr_t_to_scm (q);
  *rp = SCM_I_MAKINUM (r);
}

void
scm_integer_round_divide_iz (intptr_t x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  integer_round_divide_zz (intptr_t_to_bignum (x), y, qp, rp);
}

void
scm_integer_round_divide_zi (struct scm_bignum *x, intptr_t y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("round-divide");

  mpz_t q, zx;
  mpz_init (q);
  alias_bignum_to_mpz (x, zx);
  intptr_t r;
  int needs_adjustment;

  if (y > 0)
    {
      r = mpz_fdiv_q_ui (q, zx, y);
      if (mpz_odd_p (q))
        needs_adjustment = (2*r >= y);
      else
        needs_adjustment = (2*r > y);
    }
  else
    {
      r = - mpz_cdiv_q_ui (q, zx, -y);
      mpz_neg (q, q);
      if (mpz_odd_p (q))
        needs_adjustment = (2*r <= y);
      else
        needs_adjustment = (2*r < y);
    }
  scm_remember_upto_here_1 (x);
  if (needs_adjustment)
    {
      mpz_add_ui (q, q, 1);
      r -= y;
    }
  *qp = take_mpz (q);
  *rp = SCM_I_MAKINUM (r);
}

void
scm_integer_round_divide_zz (struct scm_bignum *x, struct scm_bignum *y, SCM *qp, SCM *rp)
{
  integer_round_divide_zz (x, y, qp, rp);
}

SCM
scm_integer_gcd_ii (intptr_t x, intptr_t y)
{
  intptr_t u = x < 0 ? -x : x;
  intptr_t v = y < 0 ? -y : y;
  intptr_t result;
  if (x == 0)
    result = v;
  else if (y == 0)
    result = u;
  else
    {
      int k = 0;
      /* Determine a common factor 2^k */
      while (((u | v) & 1) == 0)
        {
          k++;
          u >>= 1;
          v >>= 1;
        }
      /* Now, any factor 2^n can be eliminated */
      if ((u & 1) == 0)
        while ((u & 1) == 0)
          u >>= 1;
      else
        while ((v & 1) == 0)
          v >>= 1;
      /* Both u and v are now odd.  Subtract the smaller one
         from the larger one to produce an even number, remove
         more factors of two, and repeat. */
      while (u != v)
        {
          if (u > v)
            {
              u -= v;
              while ((u & 1) == 0)
                u >>= 1;
            }
          else
            {
              v -= u;
              while ((v & 1) == 0)
                v >>= 1;
            }
        }
      result = u << k;
    }
  return uintptr_t_to_scm (result);
}

SCM
scm_integer_gcd_zi (struct scm_bignum *x, intptr_t y)
{
  scm_t_bits result;
  if (y == 0)
    return scm_integer_abs_z (x);
  if (y < 0)
    y = -y;
  mpz_t zx;
  alias_bignum_to_mpz (x, zx);
  result = mpz_gcd_ui (NULL, zx, y);
  scm_remember_upto_here_1 (x);
  return uintptr_t_to_scm (result);
}

SCM
scm_integer_gcd_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_gcd (result, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (result);
}

SCM
scm_integer_lcm_ii (intptr_t x, intptr_t y)
{
  SCM d = scm_integer_gcd_ii (x, y);
  if (scm_is_eq (d, SCM_INUM0))
    return d;
  else
    return scm_abs (scm_product (SCM_I_MAKINUM (x),
                                 scm_quotient (SCM_I_MAKINUM (y), d)));
}

SCM
scm_integer_lcm_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0) return SCM_INUM0;
  if (y < 0) y = - y;
  mpz_t result, zx;
  mpz_init (result);
  alias_bignum_to_mpz (x, zx);
  mpz_lcm_ui (result, zx, y);
  scm_remember_upto_here_1 (x);
  return take_mpz (result);
}

SCM
scm_integer_lcm_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_lcm (result, zx, zy);
  scm_remember_upto_here_2 (x, y);
  /* shouldn't need to normalize b/c lcm of 2 bigs should be big */
  return take_mpz (result);
}

/* Emulating 2's complement bignums with sign magnitude arithmetic:

   Logand:
   X	Y	Result	Method:
		 (len)
   +	+	+ x	(map digit:logand X Y)
   +	-	+ x	(map digit:logand X (lognot (+ -1 Y)))
   -	+	+ y	(map digit:logand (lognot (+ -1 X)) Y)
   -	-	-	(+ 1 (map digit:logior (+ -1 X) (+ -1 Y)))

   Logior:
   X	Y	Result	Method:

   +	+	+	(map digit:logior X Y)
   +	-	- y	(+ 1 (map digit:logand (lognot X) (+ -1 Y)))
   -	+	- x	(+ 1 (map digit:logand (+ -1 X) (lognot Y)))
   -	-	- x	(+ 1 (map digit:logand (+ -1 X) (+ -1 Y)))

   Logxor:
   X	Y	Result	Method:

   +	+	+	(map digit:logxor X Y)
   +	-	-	(+ 1 (map digit:logxor X (+ -1 Y)))
   -	+	-	(+ 1 (map digit:logxor (+ -1 X) Y))
   -	-	+	(map digit:logxor (+ -1 X) (+ -1 Y))

   Logtest:
   X	Y	Result

   +	+	(any digit:logand X Y)
   +	-	(any digit:logand X (lognot (+ -1 Y)))
   -	+	(any digit:logand (lognot (+ -1 X)) Y)
   -	-	#t

*/

SCM
scm_integer_logand_ii (intptr_t x, intptr_t y)
{
  return SCM_I_MAKINUM (x & y);
}

SCM
scm_integer_logand_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    return SCM_INUM0;

  if (y > 0)
    {
      mp_limb_t rd = bignum_limbs (x)[0];
      mp_limb_t yd = y;
      if (bignum_is_negative (x))
        rd = ~rd + 1;
      scm_remember_upto_here_1 (x);
      rd &= yd;
      // Result must be a positive inum.
      return SCM_I_MAKINUM (rd);
    }

  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (x, zx);
  mpz_init_set_si (zy, y);
  mpz_and (result, zy, zx);
  scm_remember_upto_here_1 (x);
  mpz_clear (zy);
  return take_mpz (result);
}

SCM
scm_integer_logand_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_and (result, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (result);
}

SCM
scm_integer_logior_ii (intptr_t x, intptr_t y)
{
  return SCM_I_MAKINUM (x | y);
}

SCM
scm_integer_logior_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    return scm_from_bignum (x);

  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (x, zx);
  mpz_init_set_si (zy, y);
  mpz_ior (result, zy, zx);
  scm_remember_upto_here_1 (x);
  mpz_clear (zy);
  return take_mpz (result);
}

SCM
scm_integer_logior_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_ior (result, zy, zx);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (result);
}

SCM
scm_integer_logxor_ii (intptr_t x, intptr_t y)
{
  return SCM_I_MAKINUM (x ^ y);
}

SCM
scm_integer_logxor_zi (struct scm_bignum *x, intptr_t y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (x, zx);
  mpz_init_set_si (zy, y);
  mpz_xor (result, zy, zx);
  scm_remember_upto_here_1 (x);
  mpz_clear (zy);
  return take_mpz (result);
}

SCM
scm_integer_logxor_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  mpz_xor (result, zy, zx);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (result);
}

int
scm_integer_logtest_ii (intptr_t x, intptr_t y)
{
  return (x & y) ? 1 : 0;
}

int
scm_integer_logtest_zi (struct scm_bignum *x, intptr_t y)
{
  return scm_is_eq (scm_integer_logand_zi (x, y), SCM_INUM0);
}

int
scm_integer_logtest_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  return scm_is_eq (scm_integer_logand_zz (x, y), SCM_INUM0);
}

int
scm_integer_logbit_ui (uintptr_t index, intptr_t n)
{
  if (index < SCM_INTPTR_T_BIT)
    /* Assume two's complement representation.  */
    return (n >> index) & 1;
  else
    return n < 0;
}

int
scm_integer_logbit_uz (uintptr_t index, struct scm_bignum *n)
{
  mpz_t zn;
  alias_bignum_to_mpz (n, zn);
  int val = mpz_tstbit (zn, index);
  scm_remember_upto_here_1 (n);
  return val;
}

SCM
scm_integer_lognot_i (intptr_t n)
{
  return SCM_I_MAKINUM (~n);
}

SCM
scm_integer_lognot_z (struct scm_bignum *n)
{
  mpz_t result, zn;
  mpz_init (result);
  alias_bignum_to_mpz (n, zn);
  mpz_com (result, zn);
  scm_remember_upto_here_1 (n);
  return take_mpz (result);
}

SCM
scm_integer_expt_ii (intptr_t n, intptr_t k)
{
  ASSERT (k >= 0);
  if (k == 0)
    return SCM_INUM1;
  if (k == 1)
    return SCM_I_MAKINUM (n);
  if (n == -1)
    return scm_is_integer_odd_i (k) ? SCM_I_MAKINUM (-1) : SCM_INUM1;
  if (n == 2)
    {
      if (k < SCM_I_FIXNUM_BIT - 1)
        return SCM_I_MAKINUM (L1 << k);
      if (k < 64)
        return scm_integer_from_uint64 (((uint64_t) 1) << k);
      size_t nlimbs = k / (sizeof (mp_limb_t)*8) + 1;
      size_t high_shift = k & (sizeof (mp_limb_t)*8 - 1);
      struct scm_bignum *result = allocate_bignum (nlimbs);
      mp_limb_t *rd = bignum_limbs (result);
      mpn_zero(rd, nlimbs - 1);
      rd[nlimbs - 1] = ((mp_limb_t) 1) << high_shift;
      return scm_from_bignum (result);
    }

  mpz_t res;
  mpz_init (res);
  mpz_ui_pow_ui (res, inum_magnitude (n), k);
  if (n < 0 && (k & 1))
    mpz_neg (res, res);
  return take_mpz (res);
}

SCM
scm_integer_expt_zi (struct scm_bignum *n, intptr_t k)
{
  ASSERT (k >= 0);
  mpz_t res, zn;
  mpz_init (res);
  alias_bignum_to_mpz (n, zn);
  mpz_pow_ui (res, zn, k);
  scm_remember_upto_here_1 (n);
  return take_mpz (res);
}

static void
integer_init_mpz (mpz_ptr z, SCM n)
{
  if (SCM_I_INUMP (n))
    mpz_init_set_si (z, SCM_I_INUM (n));
  else
    {
      ASSERT (SCM_BIGP (n));
      mpz_t zn;
      alias_bignum_to_mpz (scm_bignum (n), zn);
      mpz_init_set (z, zn);
      scm_remember_upto_here_1 (n);
    }
}

SCM
scm_integer_modulo_expt_nnn (SCM n, SCM k, SCM m)
{
  if (scm_is_eq (m, SCM_INUM0))
    scm_num_overflow ("modulo-expt");

  mpz_t n_tmp, k_tmp, m_tmp;

  integer_init_mpz (n_tmp, n);
  integer_init_mpz (k_tmp, k);
  integer_init_mpz (m_tmp, m);

  /* if the exponent K is negative, and we simply call mpz_powm, we
     will get a divide-by-zero exception when an inverse 1/n mod m
     doesn't exist (or is not unique).  Since exceptions are hard to
     handle, we'll attempt the inversion "by hand" -- that way, we get
     a simple failure code, which is easy to handle. */

  if (-1 == mpz_sgn (k_tmp))
    {
      if (!mpz_invert (n_tmp, n_tmp, m_tmp))
        {
          mpz_clear (n_tmp);
          mpz_clear (k_tmp);
          mpz_clear (m_tmp);

          scm_num_overflow ("modulo-expt");
        }
      mpz_neg (k_tmp, k_tmp);
    }

  mpz_powm (n_tmp, n_tmp, k_tmp, m_tmp);

  if (mpz_sgn (m_tmp) < 0 && mpz_sgn (n_tmp) != 0)
    mpz_add (n_tmp, n_tmp, m_tmp);

  mpz_clear (m_tmp);
  mpz_clear (k_tmp);

  return take_mpz (n_tmp);
}

/* Efficiently compute (N * 2^COUNT), where N is an exact integer, and
   COUNT > 0. */
SCM
scm_integer_lsh_iu (intptr_t n, uintptr_t count)
{
  ASSERT (count > 0);
  /* Left shift of count >= SCM_I_FIXNUM_BIT-1 will almost[*] always
     overflow a non-zero fixnum.  For smaller shifts we check the
     bits going into positions above SCM_I_FIXNUM_BIT-1.  If they're
     all 0s for nn>=0, or all 1s for nn<0 then there's no overflow.
     Those bits are "nn >> (SCM_I_FIXNUM_BIT-1 - count)".

     [*] There's one exception:
     (-1) << SCM_I_FIXNUM_BIT-1 == SCM_MOST_NEGATIVE_FIXNUM  */

  if (n == 0)
    return SCM_I_MAKINUM (n);
  else if (count < SCM_I_FIXNUM_BIT-1 &&
           ((scm_t_bits) (SCM_SRS (n, (SCM_I_FIXNUM_BIT-1 - count)) + 1)
            <= 1))
    return SCM_I_MAKINUM (n < 0 ? -(-n << count) : (n << count));
  else
    {
      mpz_t result;
      mpz_init_set_si (result, n);
      mpz_mul_2exp (result, result, count);
      return take_mpz (result);
    }
}

SCM
scm_integer_lsh_zu (struct scm_bignum *n, uintptr_t count)
{
  ASSERT (count > 0);
  mpz_t result, zn;
  mpz_init (result);
  alias_bignum_to_mpz (n, zn);
  mpz_mul_2exp (result, zn, count);
  scm_remember_upto_here_1 (n);
  return take_mpz (result);
}

/* Efficiently compute floor (N / 2^COUNT), where N is an exact integer
   and COUNT > 0. */
SCM
scm_integer_floor_rsh_iu (intptr_t n, uintptr_t count)
{
  ASSERT (count > 0);
  if (count >= SCM_I_FIXNUM_BIT)
    return (n >= 0 ? SCM_INUM0 : SCM_I_MAKINUM (-1));
  else
    return SCM_I_MAKINUM (SCM_SRS (n, count));
}

SCM
scm_integer_floor_rsh_zu (struct scm_bignum *n, uintptr_t count)
{
  ASSERT (count > 0);
  mpz_t result, zn;
  mpz_init (result);
  alias_bignum_to_mpz (n, zn);
  mpz_fdiv_q_2exp (result, zn, count);
  scm_remember_upto_here_1 (n);
  return take_mpz (result);
}

/* Efficiently compute round (N / 2^COUNT), where N is an exact integer
   and COUNT > 0. */
SCM
scm_integer_round_rsh_iu (intptr_t n, uintptr_t count)
{
  ASSERT (count > 0);
  if (count >= SCM_I_FIXNUM_BIT)
    return SCM_INUM0;
  else
    {
      intptr_t q = SCM_SRS (n, count);

      if (0 == (n & (L1 << (count-1))))
        return SCM_I_MAKINUM (q);                /* round down */
      else if (n & ((L1 << (count-1)) - 1))
        return SCM_I_MAKINUM (q + 1);            /* round up */
      else
        return SCM_I_MAKINUM ((~L1) & (q + 1));  /* round to even */
    }
}

SCM
scm_integer_round_rsh_zu (struct scm_bignum *n, uintptr_t count)
{
  ASSERT (count > 0);
  mpz_t q, zn;
  mpz_init (q);
  alias_bignum_to_mpz (n, zn);
  mpz_fdiv_q_2exp (q, zn, count);
  if (mpz_tstbit (zn, count-1)
      && (mpz_odd_p (q) || mpz_scan1 (zn, 0) < count-1))
    mpz_add_ui (q, q, 1);
  scm_remember_upto_here_1 (n);
  return take_mpz (q);
}

#define MIN(A, B) ((A) <= (B) ? (A) : (B))

SCM
scm_integer_bit_extract_i (intptr_t n, uintptr_t start,
                           uintptr_t bits)
{
  /* When istart>=SCM_I_FIXNUM_BIT we can just limit the shift to
     SCM_I_FIXNUM_BIT-1 to get either 0 or -1 per the sign of "n". */
  n = SCM_SRS (n, MIN (start, SCM_I_FIXNUM_BIT-1));

  if (n < 0 && bits >= SCM_I_FIXNUM_BIT)
    {
      /* Since we emulate two's complement encoded numbers, this special
         case requires us to produce a result that has more bits than
         can be stored in a fixnum.  */
      mpz_t result;
      mpz_init_set_si (result, n);
      mpz_fdiv_r_2exp (result, result, bits);
      return take_mpz (result);
    }

  /* mask down to requisite bits */
  bits = MIN (bits, SCM_I_FIXNUM_BIT);
  return SCM_I_MAKINUM (n & ((L1 << bits) - 1));
}

SCM
scm_integer_bit_extract_z (struct scm_bignum *n, uintptr_t start, uintptr_t bits)
{
  mpz_t zn;
  alias_bignum_to_mpz (n, zn);

  if (bits == 1)
    {
      int bit = mpz_tstbit (zn, start);
      scm_remember_upto_here_1 (n);
      return SCM_I_MAKINUM (bit);
    }

  /* ENHANCE-ME: It'd be nice not to allocate a new bignum when
     bits<SCM_I_FIXNUM_BIT.  Would want some help from GMP to get
     such bits into a ulong.  */
  mpz_t result;
  mpz_init (result);
  mpz_fdiv_q_2exp (result, zn, start);
  mpz_fdiv_r_2exp (result, result, bits);
  scm_remember_upto_here_1 (n);
  return take_mpz (result);
}

static const char scm_logtab[] = {
  0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
};

SCM
scm_integer_logcount_i (intptr_t n)
{
  uintptr_t c = 0;
  if (n < 0)
    n = -1 - n;
  while (n)
    {
      c += scm_logtab[15 & n];
      n >>= 4;
    }
  return SCM_I_MAKINUM (c);
}

SCM
scm_integer_logcount_z (struct scm_bignum *n)
{
  uintptr_t count;
  mpz_t zn;
  alias_bignum_to_mpz (n, zn);
  if (mpz_sgn (zn) >= 0)
    count = mpz_popcount (zn);
  else
    {
      mpz_t z_negative_one;
      mpz_init_set_si (z_negative_one, -1);
      count = mpz_hamdist (zn, z_negative_one);
      mpz_clear (z_negative_one);
    }
  scm_remember_upto_here_1 (n);
  return scm_from_uintptr_t (count);
}

static const char scm_ilentab[] = {
  0, 1, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4
};

SCM
scm_integer_length_i (intptr_t n)
{
  uintptr_t c = 0;
  unsigned int l = 4;
  if (n < 0)
    n = -1 - n;
  while (n)
    {
      c += 4;
      l = scm_ilentab [15 & n];
      n >>= 4;
    }
  return SCM_I_MAKINUM (c - 4 + l);
}

SCM
scm_integer_length_z (struct scm_bignum *n)
{
  /* mpz_sizeinbase looks at the absolute value of negatives, whereas we
     want a ones-complement.  If n is ...111100..00 then mpz_sizeinbase is
     1 too big, so check for that and adjust.  */
  mpz_t zn;
  alias_bignum_to_mpz (n, zn);
  size_t size = mpz_sizeinbase (zn, 2);
  /* If negative and no 0 bits above the lowest 1, adjust result.  */
  if (mpz_sgn (zn) < 0 && mpz_scan0 (zn, mpz_scan1 (zn, 0)) == UINTPTR_MAX)
    size--;
  scm_remember_upto_here_1 (n);
  return scm_from_size_t (size);
}

SCM
scm_integer_to_string_i (intptr_t n, int base)
{
  // FIXME: Use mpn_get_str instead.
  char num_buf [SCM_INTBUFLEN];
  size_t length = scm_iint2str (n, base, num_buf);
  return scm_from_latin1_stringn (num_buf, length);
}

SCM
scm_integer_to_string_z (struct scm_bignum *n, int base)
{
  mpz_t zn;
  alias_bignum_to_mpz (n, zn);
  char *str = mpz_get_str (NULL, base, zn);
  scm_remember_upto_here_1 (n);
  size_t len = strlen (str);
  void (*freefunc) (void *, size_t);
  mp_get_memory_functions (NULL, NULL, &freefunc);
  SCM ret = scm_from_latin1_stringn (str, len);
  freefunc (str, len + 1);
  return ret;
}

int
scm_is_integer_equal_ir (intptr_t x, double y)
{
  /* On a 32-bit system an inum fits a double, we can cast the inum
     to a double and compare.

     But on a 64-bit system an inum is bigger than a double and casting
     it to a double (call that dx) will round.  Although dxx will not in
     general be equal to x, dx will always be an integer and within a
     factor of 2 of x, so if dx==y, we know that y is an integer and
     fits in scm_t_signed_bits.  So we cast y to scm_t_signed_bits and
     compare with plain x.

     An alternative (for any size system actually) would be to check y
     is an integer (with floor) and is in range of an inum (compare
     against appropriate powers of 2) then test x==(intptr_t)y.  It's
     just a matter of which casts/comparisons might be fastest or
     easiest for the cpu.  */
  return (double) x == y
    && (DBL_MANT_DIG >= SCM_I_FIXNUM_BIT-1 || x == (intptr_t) y);
}

int
scm_is_integer_equal_ic (intptr_t x, double real, double imag)
{
  return imag == 0.0 && scm_is_integer_equal_ir (x, real);
}

int
scm_is_integer_equal_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t zx, zy;
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  int cmp = mpz_cmp (zx, zy);
  scm_remember_upto_here_2 (x, y);
  return 0 == cmp;
}

int
scm_is_integer_equal_zr (struct scm_bignum *x, double y)
{
  if (isnan (y))
    return 0;
  mpz_t zx;
  alias_bignum_to_mpz (x, zx);
  int cmp = mpz_cmp_d (zx, y);
  scm_remember_upto_here_1 (x);
  return 0 == cmp;
}

int
scm_is_integer_equal_zc (struct scm_bignum *x, double real, double imag)
{
  return imag == 0.0 && scm_is_integer_equal_zr (x, real);
}

int
scm_is_integer_less_than_ir (intptr_t x, double y)
{
  /* We can safely take the ceiling of y without changing the
     result of x<y, given that x is an integer. */
  y = ceil (y);

  /* In the following comparisons, it's important that the right
     hand side always be a power of 2, so that it can be
     losslessly converted to a double even on 64-bit
     machines. */
  if (y >= (double) (SCM_MOST_POSITIVE_FIXNUM+1))
    return 1;
  else if (!(y > (double) SCM_MOST_NEGATIVE_FIXNUM))
    /* The condition above is carefully written to include the
       case where y==NaN. */
    return 0;
  else
    /* y is a finite integer that fits in an inum. */
    return x < (intptr_t) y;
}

int
scm_is_integer_less_than_ri (double x, intptr_t y)
{
  /* We can safely take the floor of x without changing the
     result of x<y, given that y is an integer. */
  x = floor (x);

  /* In the following comparisons, it's important that the right
     hand side always be a power of 2, so that it can be
     losslessly converted to a double even on 64-bit
     machines. */
  if (x < (double) SCM_MOST_NEGATIVE_FIXNUM)
    return 1;
  else if (!(x < (double) (SCM_MOST_POSITIVE_FIXNUM+1)))
    /* The condition above is carefully written to include the
       case where x==NaN. */
    return 0;
  else
    /* x is a finite integer that fits in an inum. */
    return (intptr_t) x < y;
}

int
scm_is_integer_less_than_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t zx, zy;
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  int cmp = mpz_cmp (zx, zy);
  scm_remember_upto_here_2 (x, y);
  return cmp < 0;
}

int
scm_is_integer_less_than_zr (struct scm_bignum *x, double y)
{
  if (isnan (y))
    return 0;
  mpz_t zx;
  alias_bignum_to_mpz (x, zx);
  int cmp = mpz_cmp_d (zx, y);
  scm_remember_upto_here_1 (x);
  return cmp < 0;
}

int
scm_is_integer_less_than_rz (double x, struct scm_bignum *y)
{
  if (isnan (x))
    return 0;
  mpz_t zy;
  alias_bignum_to_mpz (y, zy);
  int cmp = mpz_cmp_d (zy, x);
  scm_remember_upto_here_1 (y);
  return cmp > 0;
}

int
scm_is_integer_positive_z (struct scm_bignum *x)
{
  return bignum_is_positive (x);
}

int
scm_is_integer_negative_z (struct scm_bignum *x)
{
  return bignum_is_negative (x);
}

#if SCM_ENABLE_MINI_GMP
static double
mpz_get_d_2exp (intptr_t *exp, mpz_srcptr z)
{
  double signif = mpz_get_d (z);
  int iexp;
  signif = frexp (signif, &iexp);
  *exp = iexp;
  return signif;
}
#endif

double
scm_integer_frexp_z (struct scm_bignum *x, intptr_t *exp)
{
  mpz_t zx;
  alias_bignum_to_mpz (x, zx);

  size_t bits = mpz_sizeinbase (zx, 2);
  ASSERT (bits != 0);
  size_t shift = 0;
  if (bits > DBL_MANT_DIG)
    {
      shift = bits - DBL_MANT_DIG;
      SCM xx = scm_integer_round_rsh_zu (x, shift);
      if (SCM_I_INUMP (xx))
        {
          int expon;
          double signif = frexp (SCM_I_INUM (xx), &expon);
          *exp = expon + shift;
          return signif;
        }
      x = scm_bignum (xx);
      alias_bignum_to_mpz (x, zx);
    }

  double significand = mpz_get_d_2exp (exp, zx);
  scm_remember_upto_here_1 (x);
  *exp += shift;
  return significand;
}

double
scm_integer_to_double_z (struct scm_bignum *x)
{
  intptr_t exponent;
  double significand = scm_integer_frexp_z (x, &exponent);
  return ldexp (significand, exponent);
}

SCM
scm_integer_from_double (double val)
{
  if (!isfinite (val))
    scm_out_of_range ("inexact->exact", scm_from_double (val));

  if (((double) INT64_MIN) <= val && val <= ((double) INT64_MAX))
    return scm_from_int64 (val);

  mpz_t result;
  mpz_init_set_d (result, val);
  return take_mpz (result);
}

SCM
scm_integer_add_ii (intptr_t x, intptr_t y)
{
  return intptr_t_to_scm (x + y);
}

static SCM
do_add_1 (int negative, mp_limb_t *xd, size_t xn, mp_limb_t y)
{
  size_t rn = xn + 1;
  struct scm_bignum *result = allocate_bignum (rn);
  mp_limb_t *rd = bignum_limbs (result);
  if (mpn_add_1 (rd, xd, xn, y))
    rd[xn] = 1;
  else
    result->u.z.size--;
  // No need to normalize as magnitude is increasing and one operand
  // already a bignum.
  return scm_from_bignum (bignum_negate_if (negative, result));
}

static SCM
do_add (int negative, mp_limb_t *xd, size_t xn, mp_limb_t *yd, size_t yn)
{
  size_t rn = xn + 1;
  struct scm_bignum *result = allocate_bignum (rn);
  mp_limb_t *rd = bignum_limbs (result);
  if (mpn_add (rd, xd, xn, yd, yn))
    rd[xn] = 1;
  else
    result->u.z.size--;
  // No need to normalize as magnitude is increasing and one operand
  // already a bignum.
  return scm_from_bignum (bignum_negate_if (negative, result));
}

static SCM
do_sub_1 (int negative, mp_limb_t *xd, size_t xn, mp_limb_t y)
{
  size_t rn = xn;
  struct scm_bignum *result = allocate_bignum (rn);
  mp_limb_t *rd = bignum_limbs (result);
  mpn_sub_1 (rd, xd, xn, y);
  return normalize_bignum
    (bignum_negate_if (negative, (bignum_trim1 (result))));
}

static SCM
do_sub (int negative, mp_limb_t *xd, size_t xn, mp_limb_t *yd, size_t yn)
{
  size_t rn = xn;
  struct scm_bignum *result = allocate_bignum (rn);
  mp_limb_t *rd = bignum_limbs (result);
  mpn_sub (rd, xd, xn, yd, yn);
  return normalize_bignum
    (bignum_negate_if (negative, (bignum_trimn (result))));
}

static int
do_cmp (mp_limb_t *xd, size_t xn, mp_limb_t *yd, size_t yn)
{
  if (xn < yn)
    return -1;
  if (xn > yn)
    return 1;
  return mpn_cmp (xd, yd, xn);
}

SCM
scm_integer_add_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    return scm_from_bignum (x);
  size_t xn = bignum_limb_count (x);
  if (xn == 0)
    return SCM_I_MAKINUM (y);

  SCM ret;
  if (bignum_is_negative (x) == (y < 0))
    // Magnitude increases, sign stays the same.
    ret = do_add_1 (y < 0, bignum_limbs (x), xn, inum_magnitude (y));
  else
    // Magnitude decreases, but assuming x's magnitude is greater than
    // y's, not changing sign.
    ret = do_sub_1 (bignum_is_negative (x), bignum_limbs (x), xn,
                    inum_magnitude (y));
  scm_remember_upto_here_1 (x);
  return ret;
}

SCM
scm_integer_add_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  size_t xn = bignum_limb_count (x);
  size_t yn = bignum_limb_count (y);
  if (xn == 0)
    return normalize_bignum (y);
  if (yn == 0)
    return normalize_bignum (x);

  mp_limb_t *xd = bignum_limbs (x);
  mp_limb_t *yd = bignum_limbs (y);
  SCM ret;
  if (bignum_is_negative (x) == bignum_is_negative (y))
    // Magnitude increases, sign stays the same.
    ret = xn < yn
      ? do_add (bignum_is_negative (x), yd, yn, xd, xn)
      : do_add (bignum_is_negative (x), xd, xn, yd, yn);
  else
    // Magnitude decreases, changing sign if abs(x) < abs(y).
    ret = do_cmp (xd, xn, yd, yn) < 0
      ? do_sub (!bignum_is_negative (x), yd, yn, xd, xn)
      : do_sub (bignum_is_negative (x), xd, xn, yd, yn);

  scm_remember_upto_here_2 (x, y);
  return ret;
}

SCM
scm_integer_negate_i (intptr_t x)
{
  return intptr_t_to_scm (-x);
}

SCM
scm_integer_negate_z (struct scm_bignum *x)
{
  /* Must normalize here because -SCM_MOST_NEGATIVE_FIXNUM is a bignum,
     but negating that gives a fixnum.  */
  return normalize_bignum (negate_bignum (clone_bignum (x)));
}

SCM
scm_integer_sub_ii (intptr_t x, intptr_t y)
{
  // Assumes that -INUM_MIN can fit in a intptr_t, even if that
  // intptr_t is not fixable, and that scm_integer_add_ii can handle
  // intptr_t inputs outside the fixable range.
  return scm_integer_add_ii (x, -y);
}

SCM
scm_integer_sub_iz (intptr_t x, struct scm_bignum *y)
{
  if (x == 0)
    return scm_integer_negate_z (y);
  size_t yn = bignum_limb_count (y);
  if (yn == 0)
    return SCM_I_MAKINUM (x);

  SCM ret;
  if (bignum_is_negative (y) == (x < 0))
    // Magnitude of result smaller than that of y, but assuming y's
    // magnitude is greater than x's, keeping y's sign.
    ret = do_sub_1 (x > 0, bignum_limbs (y), yn, inum_magnitude (x));
  else
    // Magnitude increases, same sign as x.
    ret = do_add_1 (x < 0, bignum_limbs (y), yn, inum_magnitude (x));
  scm_remember_upto_here_1 (y);
  return ret;
}

SCM
scm_integer_sub_zi (struct scm_bignum *x, intptr_t y)
{
  if (y == 0)
    return scm_from_bignum (x);
  size_t xn = bignum_limb_count (x);
  if (xn == 0)
    return SCM_I_MAKINUM (y);

  SCM ret;
  if (bignum_is_negative (x) == (y < 0))
    // Magnitude decreases, but assuming x's magnitude is greater than
    // y's, not changing sign.
    ret = do_sub_1 (y < 0, bignum_limbs (x), xn, inum_magnitude (y));
  else
    // Magnitude increases, same sign as x.
    ret = do_add_1 (bignum_is_negative (x), bignum_limbs (x), xn,
                    inum_magnitude (y));
  scm_remember_upto_here_1 (x);
  return ret;
}

SCM
scm_integer_sub_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  size_t xn = bignum_limb_count (x);
  size_t yn = bignum_limb_count (y);
  if (xn == 0)
    return scm_integer_negate_z (y);
  if (yn == 0)
    return scm_from_bignum (x);

  mp_limb_t *xd = bignum_limbs (x);
  mp_limb_t *yd = bignum_limbs (y);
  SCM ret;
  if (bignum_is_negative (x) != bignum_is_negative (y))
    // Magnitude increases, same sign as x.
    ret = xn < yn
      ? do_add (bignum_is_negative (x), yd, yn, xd, xn)
      : do_add (bignum_is_negative (x), xd, xn, yd, yn);
  else
    // Magnitude decreases, changing sign if abs(x) < abs(y).
    ret = do_cmp (xd, xn, yd, yn) < 0
      ? do_sub (!bignum_is_negative (x), yd, yn, xd, xn)
      : do_sub (bignum_is_negative (x), xd, xn, yd, yn);

  scm_remember_upto_here_2 (x, y);
  return ret;
}

SCM
scm_integer_mul_ii (intptr_t x, intptr_t y)
{
#if SCM_I_FIXNUM_BIT < 32
  int64_t k = x * (int64_t) y;
  if (SCM_FIXABLE (k))
    return SCM_I_MAKINUM (k);
#endif

  mp_limb_t xd[1] = { intptr_t_magnitude (x) };
  mp_limb_t lo;
  int negative = (x < 0) != (y < 0);
  mp_limb_t hi = mpn_mul_1 (&lo, xd, 1, intptr_t_magnitude (y));
  if (!hi)
    {
      if (negative)
        {
          if (lo <= intptr_t_magnitude (SCM_MOST_NEGATIVE_FIXNUM))
            return SCM_I_MAKINUM (negative_intptr_t (lo));
        }
      else if (lo <= SCM_MOST_POSITIVE_FIXNUM)
        return SCM_I_MAKINUM (lo);
      return scm_from_bignum (make_bignum_1 (negative, lo));
    }

  return scm_from_bignum (make_bignum_2 (negative, lo, hi));
}

SCM
scm_integer_mul_zi (struct scm_bignum *x, intptr_t y)
{
  switch (y)
    {
    case -1:
      return scm_integer_negate_z (x);
    case 0:
      return SCM_INUM0;
    case 1:
      return scm_from_bignum (x);
    default:
      {
        size_t xn = bignum_limb_count (x);
        if (xn == 0)
          return SCM_INUM0;

        struct scm_bignum *result = allocate_bignum (xn + 1);
        mp_limb_t *rd = bignum_limbs (result);
        const mp_limb_t *xd = bignum_limbs (x);
        mp_limb_t yd = intptr_t_magnitude (y);
        int negate = bignum_is_negative (x) != (y < 0);
        mp_limb_t hi = mpn_mul_1 (rd, xd, xn, yd);
        if (hi)
          rd[xn] = hi;
        else
          result->u.z.size--;
        scm_remember_upto_here_1 (x);
        return normalize_bignum (bignum_negate_if (negate, (result)));
      }
    }
}

SCM
scm_integer_mul_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  size_t xn = bignum_limb_count (x);
  size_t yn = bignum_limb_count (y);
  if (xn == 0 || yn == 0)
    return SCM_INUM0;

  struct scm_bignum *result = allocate_bignum (xn + yn);
  mp_limb_t *rd = bignum_limbs (result);
  const mp_limb_t *xd = bignum_limbs (x);
  const mp_limb_t *yd = bignum_limbs (y);
  int negate = bignum_is_negative (x) != bignum_is_negative (y);
  if (xd == yd)
    mpn_sqr (rd, xd, xn);
  else if (xn <= yn)
    mpn_mul (rd, yd, yn, xd, xn);
  else
    mpn_mul (rd, xd, xn, yd, yn);
  scm_remember_upto_here_2 (x, y);
  return normalize_bignum
    (bignum_negate_if (negate, (bignum_trim1 (result))));
}

int
scm_is_integer_divisible_ii (intptr_t x, intptr_t y)
{
  ASSERT (y != 0);
  return (x % y) == 0;
}

int
scm_is_integer_divisible_zi (struct scm_bignum *x, intptr_t y)
{
  ASSERT (y != 0);
  switch (y)
    {
    case -1:
    case 1:
      return 1;
    default:
      {
        intptr_t abs_y = y < 0 ? -y : y;
        mpz_t zx;
        alias_bignum_to_mpz (x, zx);
        int divisible = mpz_divisible_ui_p (zx, abs_y);
        scm_remember_upto_here_1 (x);
        return divisible;
      }
    }
}

int
scm_is_integer_divisible_zz (struct scm_bignum *x, struct scm_bignum *y)
{
  mpz_t zx, zy;
  alias_bignum_to_mpz (x, zx);
  alias_bignum_to_mpz (y, zy);
  int divisible_p = mpz_divisible_p (zx, zy);
  scm_remember_upto_here_2 (x, y);
  return divisible_p;
}

SCM
scm_integer_exact_quotient_ii (intptr_t n, intptr_t d)
{
  return scm_integer_truncate_quotient_ii (n, d);
}

SCM
scm_integer_exact_quotient_iz (intptr_t n, struct scm_bignum *d)
{
  // There are only two fixnum numerators that are evenly divided by
  // bignum denominators: 0, which is evenly divided 0 times by
  // anything, and SCM_MOST_NEGATIVE_FIXNUM, which is evenly divided -1
  // time by SCM_MOST_POSITIVE_FIXNUM+1.
  if (n == 0)
    return SCM_INUM0;
  ASSERT (n == SCM_MOST_NEGATIVE_FIXNUM);
  ASSERT (bignum_cmp_intptr_t (d, SCM_MOST_POSITIVE_FIXNUM + 1) == 0);
  return SCM_I_MAKINUM (-1);
}

/* Return the exact integer q such that n = q*d, for exact integers n
   and d, where d is known in advance to divide n evenly (with zero
   remainder).  For large integers, this can be computed more
   efficiently than when the remainder is unknown. */
SCM
scm_integer_exact_quotient_zi (struct scm_bignum *n, intptr_t d)
{
  if (SCM_UNLIKELY (d == 0))
    scm_num_overflow ("quotient");
  else if (SCM_UNLIKELY (d == 1))
    return scm_from_bignum (n);

  mpz_t q, zn;
  mpz_init (q);
  alias_bignum_to_mpz (n, zn);
  if (d > 0)
    mpz_divexact_ui (q, zn, d);
  else
    {
      mpz_divexact_ui (q, zn, -d);
      mpz_neg (q, q);
    }
  scm_remember_upto_here_1 (n);
  return take_mpz (q);
}

SCM
scm_integer_exact_quotient_zz (struct scm_bignum *n, struct scm_bignum *d)
{
  mpz_t q, zn, zd;
  mpz_init (q);
  alias_bignum_to_mpz (n, zn);
  alias_bignum_to_mpz (d, zd);

  mpz_divexact (q, zn, zd);
  scm_remember_upto_here_2 (n, d);
  return take_mpz (q);
}

#if SCM_SIZEOF_INTPTR_T == 4
SCM
scm_integer_from_int32 (int32_t n)
{
  if (SCM_FIXABLE (n))
    return SCM_I_MAKINUM (n);
  return scm_from_bignum (intptr_t_to_bignum (n));
}

SCM
scm_integer_from_uint32 (uint32_t n)
{
  if (SCM_POSFIXABLE (n))
    return SCM_I_MAKINUM (n);
  return scm_from_bignum (uintptr_t_to_bignum (n));
}

int
scm_integer_to_int32_z (struct scm_bignum *z, int32_t *val)
{
  return bignum_to_int32 (z, val);
}

int
scm_integer_to_uint32_z (struct scm_bignum *z, uint32_t *val)
{
  return bignum_to_uint32 (z, val);
}
#endif

SCM
scm_integer_from_int64 (int64_t n)
{
  if (SCM_FIXABLE (n))
    return SCM_I_MAKINUM (n);
  return scm_from_bignum (make_bignum_from_int64 (n));
}

SCM
scm_integer_from_uint64 (uint64_t n)
{
  if (SCM_POSFIXABLE (n))
    return SCM_I_MAKINUM (n);
  return scm_from_bignum (make_bignum_from_uint64 (n));
}

int
scm_integer_to_int64_z (struct scm_bignum *z, int64_t *val)
{
  return bignum_to_int64 (z, val);
}

int
scm_integer_to_uint64_z (struct scm_bignum *z, uint64_t *val)
{
  return bignum_to_uint64 (z, val);
}

void
scm_integer_set_mpz_z (struct scm_bignum *z, mpz_t n)
{
  mpz_t zn;
  alias_bignum_to_mpz (z, zn);
  mpz_set (n, zn);
  scm_remember_upto_here_1 (z);
}

void
scm_integer_init_set_mpz_z (struct scm_bignum *z, mpz_t n)
{
  mpz_init (n);
  scm_integer_set_mpz_z (z, n);
}

void
scm_integer_exact_sqrt_i (intptr_t k, SCM *s, SCM *r)
{
  ASSERT (k >= 0);
  if (k == 0)
    *s = *r = SCM_INUM0;
  else
    {
      mp_limb_t kk = k, ss, rr;
      if (mpn_sqrtrem (&ss, &rr, &kk, 1) == 0)
        rr = 0;
      *s = SCM_I_MAKINUM (ss);
      *r = SCM_I_MAKINUM (rr);
    }
}

void
scm_integer_exact_sqrt_z (struct scm_bignum *k, SCM *s, SCM *r)
{
  mpz_t zk, zs, zr;
  alias_bignum_to_mpz (k, zk);
  mpz_init (zs);
  mpz_init (zr);

  mpz_sqrtrem (zs, zr, zk);
  scm_remember_upto_here_1 (k);
  *s = take_mpz (zs);
  *r = take_mpz (zr);
}

int
scm_is_integer_perfect_square_i (intptr_t k)
{
  if (k < 0)
    return 0;
  if (k == 0)
    return 1;
  mp_limb_t kk = k;
  return mpn_perfect_square_p (&kk, 1);
}

int
scm_is_integer_perfect_square_z (struct scm_bignum *k)
{
  mpz_t zk;
  alias_bignum_to_mpz (k, zk);
  int result = mpz_perfect_square_p (zk);
  scm_remember_upto_here_1 (k);
  return result;
}

SCM
scm_integer_floor_sqrt_i (intptr_t k)
{
  if (k <= 0)
    return SCM_INUM0;

  mp_limb_t kk = k, ss;
  mpn_sqrtrem (&ss, NULL, &kk, 1);
  return SCM_I_MAKINUM (ss);
}

SCM
scm_integer_floor_sqrt_z (struct scm_bignum *k)
{
  mpz_t zk, zs;
  alias_bignum_to_mpz (k, zk);
  mpz_init (zs);
  mpz_sqrt (zs, zk);
  scm_remember_upto_here_1 (k);
  return take_mpz (zs);
}

double
scm_integer_inexact_sqrt_i (intptr_t k)
{
  if (k < 0)
    return -sqrt ((double) -k);
  return sqrt ((double) k);
}

double
scm_integer_inexact_sqrt_z (struct scm_bignum *k)
{
  intptr_t expon;
  double signif = scm_integer_frexp_z (k, &expon);
  int negative = signif < 0;
  if (negative)
    signif = -signif;

  if (expon & 1)
    {
      signif *= 2;
      expon--;
    }
  double result = ldexp (sqrt (signif), expon / 2);
  return negative ? -result : result;
}

SCM
scm_integer_scan1_i (intptr_t n)
{
  if (n == 0)
    return SCM_I_MAKINUM (-1);
  n = n ^ (n-1);  /* 1 bits for each low 0 and lowest 1 */
  return scm_integer_logcount_i (n >> 1);
}

SCM
scm_integer_scan1_z (struct scm_bignum *n)
{
  mpz_t zn;
  alias_bignum_to_mpz (n, zn);
  uintptr_t pos = mpz_scan1 (zn, 0L);
  scm_remember_upto_here_1 (n);
  return uintptr_t_to_scm (pos);
}
