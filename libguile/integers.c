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

#include "boolean.h"
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

static struct scm_bignum *
make_bignum_1 (int is_negative, mp_limb_t limb)
{
  struct scm_bignum *z = allocate_bignum (1);
  z->limbs[0] = limb;
  return is_negative ? negate_bignum(z) : z;
}

static struct scm_bignum *
ulong_to_bignum (unsigned long u)
{
  return make_bignum_1 (0, u);
};

static struct scm_bignum *
long_to_bignum (long i)
{
  if (i > 0)
    return ulong_to_bignum (i);

  return make_bignum_1 (1, long_magnitude (i));
};

static SCM
long_to_scm (long i)
{
  if (SCM_FIXABLE (i))
    return SCM_I_MAKINUM (i);
  return SCM_PACK (long_to_bignum (i));
}

static SCM
ulong_to_scm (unsigned long i)
{
  if (SCM_POSFIXABLE (i))
    return SCM_I_MAKINUM (i);
  return SCM_PACK (ulong_to_bignum (i));
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

static SCM
take_mpz (mpz_ptr mpz)
{
  struct scm_bignum *res = make_bignum_from_mpz (mpz);
  mpz_clear (mpz);
  return normalize_bignum (res);
}

static int
long_sign (long l)
{
  if (l < 0) return -1;
  if (l == 0) return 0;
  return 1;
}

static int
bignum_cmp_long (struct scm_bignum *z, long l)
{
  switch (bignum_size (z))
    {
    case -1:
      if (l >= 0)
        return -1;
      return long_sign (long_magnitude (l) - bignum_limbs (z)[0]);
    case 0:
      return long_sign (l);
    case 1:
      if (l <= 0)
        return 1;
      return long_sign (bignum_limbs (z)[0] - (unsigned long) l);
    default:
      return long_sign (bignum_size (z));
    }
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

SCM
scm_integer_abs_i (scm_t_inum i)
{
  if (i >= 0)
    return SCM_I_MAKINUM (i);

  return ulong_to_scm (long_magnitude (i));
}

SCM
scm_integer_abs_z (SCM z)
{
  if (!bignum_is_negative (scm_bignum (z)))
    return z;

  return SCM_PACK (negate_bignum (clone_bignum (scm_bignum (z))));
}

SCM
scm_integer_floor_quotient_ii (scm_t_inum x, scm_t_inum y)
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
  scm_t_inum q = x / y;
  return long_to_scm (q);
}

SCM
scm_integer_floor_quotient_iz (scm_t_inum x, SCM y)
{
  if (x == 0 || ((x < 0) == bignum_is_negative (scm_bignum (y))))
    return SCM_INUM0;
  return SCM_I_MAKINUM (-1);
}
 
SCM
scm_integer_floor_quotient_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("floor-quotient");
  else if (y == 1)
    return x;

  mpz_t zx, q;
  alias_bignum_to_mpz (scm_bignum (x), zx);
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
scm_integer_floor_quotient_zz (SCM x, SCM y)
{
  mpz_t zx, zy, q;
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_init (q);
  mpz_fdiv_q (q, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (q);
}

SCM
scm_integer_floor_remainder_ii (scm_t_inum x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("floor-remainder");
  scm_t_inum r = x % y;
  int needs_adjustment = (y > 0) ? (r < 0) : (r > 0);
  if (needs_adjustment)
    r += y;
  return SCM_I_MAKINUM (r);
}

SCM
scm_integer_floor_remainder_iz (scm_t_inum x, SCM y)
{
  if (bignum_is_positive (scm_bignum (y)))
    {
      if (x < 0)
        {
          mpz_t r, zy;
          mpz_init (r);
          alias_bignum_to_mpz (scm_bignum (y), zy);
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
      alias_bignum_to_mpz (scm_bignum (y), zy);
      mpz_add_ui (r, zy, x);
      scm_remember_upto_here_1 (y);
      return take_mpz (r);
    }
}

SCM
scm_integer_floor_remainder_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("floor-remainder");
  else
    {
      scm_t_inum r;
      mpz_t zx;
      alias_bignum_to_mpz (scm_bignum (x), zx);
      if (y > 0)
        r = mpz_fdiv_ui (zx, y);
      else
        r = -mpz_cdiv_ui (zx, -y);
      scm_remember_upto_here_1 (x);
      return SCM_I_MAKINUM (r);
    }
}

SCM
scm_integer_floor_remainder_zz (SCM x, SCM y)
{
  mpz_t zx, zy, r;
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_init (r);
  mpz_fdiv_r (r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (r);
}

void
scm_integer_floor_divide_ii (scm_t_inum x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("floor-divide");

  scm_t_inum q = x / y;
  scm_t_inum r = x % y;
  int needs_adjustment = (y > 0) ? (r < 0) : (r > 0);

  if (needs_adjustment)
    {
      r += y;
      q--;
    }

  *qp = long_to_scm (q);
  *rp = SCM_I_MAKINUM (r);
}

void
scm_integer_floor_divide_iz (scm_t_inum x, SCM y, SCM *qp, SCM *rp)
{
  if (bignum_is_positive (scm_bignum (y)))
    {
      if (x < 0)
        {
          mpz_t zy, r;
          alias_bignum_to_mpz (scm_bignum (y), zy);
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
      alias_bignum_to_mpz (scm_bignum (y), zy);
      mpz_init (r);
      mpz_add_ui (r, zy, x);
      scm_remember_upto_here_1 (y);
      *qp = SCM_I_MAKINUM (-1);
      *rp = take_mpz (r);
    }
}

void
scm_integer_floor_divide_zi (SCM x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("floor-divide");

  mpz_t zx, q, r;
  alias_bignum_to_mpz (scm_bignum (x), zx);
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
scm_integer_floor_divide_zz (SCM x, SCM y, SCM *qp, SCM *rp)
{
  mpz_t zx, zy, q, r;
  mpz_init (q);
  mpz_init (r);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_fdiv_qr (q, r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  *qp = take_mpz (q);
  *rp = take_mpz (r);
}

SCM
scm_integer_ceiling_quotient_ii (scm_t_inum x, scm_t_inum y)
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
  scm_t_inum q = x / y;

  return long_to_scm (q);
}

SCM
scm_integer_ceiling_quotient_iz (scm_t_inum x, SCM y)
{
  if (bignum_is_positive (scm_bignum (y)))
    {
      if (x > 0)
        return SCM_INUM1;
      else if (x == SCM_MOST_NEGATIVE_FIXNUM &&
               bignum_cmp_long (scm_bignum (y), -SCM_MOST_NEGATIVE_FIXNUM) == 0)
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
scm_integer_ceiling_quotient_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("ceiling-quotient");
  else if (y == 1)
    return x;
  else
    {
      mpz_t q, zx;
      mpz_init (q);
      alias_bignum_to_mpz (scm_bignum (x), zx);
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
scm_integer_ceiling_quotient_zz (SCM x, SCM y)
{
  mpz_t q, zx, zy;
  mpz_init (q);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_cdiv_q (q, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (q);
}

SCM
scm_integer_ceiling_remainder_ii (scm_t_inum x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("ceiling-remainder");

  scm_t_inum r = x % y;
  int needs_adjustment = (y > 0) ? (r > 0) : (r < 0);
  if (needs_adjustment)
    r -= y;

  return SCM_I_MAKINUM (r);
}

SCM
scm_integer_ceiling_remainder_iz (scm_t_inum x, SCM y)
{
  if (bignum_is_positive (scm_bignum (y)))
    {
      if (x > 0)
        {
          mpz_t r, zy;
          mpz_init (r);
          alias_bignum_to_mpz (scm_bignum (y), zy);
          mpz_sub_ui (r, zy, x);
          scm_remember_upto_here_1 (y);
          mpz_neg (r, r);
          return take_mpz (r);
        }
      else if (x == SCM_MOST_NEGATIVE_FIXNUM &&
               bignum_cmp_long (scm_bignum (y), -SCM_MOST_NEGATIVE_FIXNUM) == 0)
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
      alias_bignum_to_mpz (scm_bignum (y), zy);
      mpz_add_ui (r, zy, -x);
      scm_remember_upto_here_1 (y);
      mpz_neg (r, r);
      return take_mpz (r);
    }
}

SCM
scm_integer_ceiling_remainder_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("ceiling-remainder");
  else
    {
      mpz_t zx;
      alias_bignum_to_mpz (scm_bignum (x), zx);
      scm_t_inum r;
      if (y > 0)
        r = -mpz_cdiv_ui (zx, y);
      else
        r = mpz_fdiv_ui (zx, -y);
      scm_remember_upto_here_1 (x);
      return SCM_I_MAKINUM (r);
    }
}

SCM
scm_integer_ceiling_remainder_zz (SCM x, SCM y)
{
  mpz_t r, zx, zy;
  mpz_init (r);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_cdiv_r (r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (r);
}

void
scm_integer_ceiling_divide_ii (scm_t_inum x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("ceiling-divide");
  else
    {
      scm_t_inum q = x / y;
      scm_t_inum r = x % y;
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
      *qp = long_to_scm (q);
      *rp = SCM_I_MAKINUM (r);
    }
}

void
scm_integer_ceiling_divide_iz (scm_t_inum x, SCM y, SCM *qp, SCM *rp)
{
  if (bignum_is_positive (scm_bignum (y)))
    {
      if (x > 0)
        {
          mpz_t r, zy;
          mpz_init (r);
          alias_bignum_to_mpz (scm_bignum (y), zy);
          mpz_sub_ui (r, zy, x);
          scm_remember_upto_here_1 (y);
          mpz_neg (r, r);
          *qp = SCM_INUM1;
          *rp = take_mpz (r);
        }
      else if (x == SCM_MOST_NEGATIVE_FIXNUM &&
               bignum_cmp_long (scm_bignum (y), -SCM_MOST_NEGATIVE_FIXNUM) == 0)
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
      alias_bignum_to_mpz (scm_bignum (y), zy);
      mpz_add_ui (r, zy, -x);
      scm_remember_upto_here_1 (y);
      mpz_neg (r, r);
      *qp = SCM_INUM1;
      *rp = take_mpz (r);
    }
}

void
scm_integer_ceiling_divide_zi (SCM x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("ceiling-divide");
  else
    {
      mpz_t q, r, zx;
      mpz_init (q);
      mpz_init (r);
      alias_bignum_to_mpz (scm_bignum (x), zx);
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
scm_integer_ceiling_divide_zz (SCM x, SCM y, SCM *qp, SCM *rp)
{
  mpz_t q, r, zx, zy;
  mpz_init (q);
  mpz_init (r);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_cdiv_qr (q, r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  *qp = take_mpz (q);
  *rp = take_mpz (r);
}

SCM
scm_integer_truncate_quotient_ii (scm_t_inum x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("truncate-quotient");
  else
    {
      scm_t_inum q = x / y;
      return long_to_scm (q);
    }
}

SCM
scm_integer_truncate_quotient_iz (scm_t_inum x, SCM y)
{
  if (x == SCM_MOST_NEGATIVE_FIXNUM &&
      bignum_cmp_long (scm_bignum (y), -SCM_MOST_NEGATIVE_FIXNUM) == 0)
    {
      /* Special case: x == fixnum-min && y == abs (fixnum-min) */
      scm_remember_upto_here_1 (y);
      return SCM_I_MAKINUM (-1);
    }
  else
    return SCM_INUM0;
}

SCM
scm_integer_truncate_quotient_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("truncate-quotient");
  else if (y == 1)
    return x;
  else
    {
      mpz_t q, zx;
      mpz_init (q);
      alias_bignum_to_mpz (scm_bignum (x), zx);
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
scm_integer_truncate_quotient_zz (SCM x, SCM y)
{
  mpz_t q, zx, zy;
  mpz_init (q);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_tdiv_q (q, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (q);
}

SCM
scm_integer_truncate_remainder_ii (scm_t_inum x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("truncate-remainder");
  else
    {
      scm_t_inum q = x % y;
      return long_to_scm (q);
    }
}

SCM
scm_integer_truncate_remainder_iz (scm_t_inum x, SCM y)
{
  if (x == SCM_MOST_NEGATIVE_FIXNUM &&
      bignum_cmp_long (scm_bignum (y), -SCM_MOST_NEGATIVE_FIXNUM) == 0)
    {
      /* Special case: x == fixnum-min && y == abs (fixnum-min) */
      scm_remember_upto_here_1 (y);
      return SCM_INUM0;
    }
  else
    return SCM_I_MAKINUM (x);
}

SCM
scm_integer_truncate_remainder_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("truncate-remainder");
  else
    {
      mpz_t zx;
      alias_bignum_to_mpz (scm_bignum (x), zx);
      scm_t_inum r = mpz_tdiv_ui (zx, (y > 0) ? y : -y) * mpz_sgn (zx);
      scm_remember_upto_here_1 (x);
      return SCM_I_MAKINUM (r);
    }
}

SCM
scm_integer_truncate_remainder_zz (SCM x, SCM y)
{
  mpz_t r, zx, zy;
  mpz_init (r);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_tdiv_r (r, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (r);
}

void
scm_integer_truncate_divide_ii (scm_t_inum x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("truncate-divide");
  else
    {
      scm_t_inum q = x / y;
      scm_t_inum r = x % y;
      *qp = long_to_scm (q);
      *rp = SCM_I_MAKINUM (r);
    }
}

void
scm_integer_truncate_divide_iz (scm_t_inum x, SCM y, SCM *qp, SCM *rp)
{
  if (x == SCM_MOST_NEGATIVE_FIXNUM &&
      bignum_cmp_long (scm_bignum (y), -SCM_MOST_NEGATIVE_FIXNUM) == 0)
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
scm_integer_truncate_divide_zi (SCM x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("truncate-divide");
  else
    {
      mpz_t q, zx;
      mpz_init (q);
      alias_bignum_to_mpz (scm_bignum (x), zx);
      scm_t_inum r;
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
scm_integer_truncate_divide_zz (SCM x, SCM y, SCM *qp, SCM *rp)
{
  mpz_t q, r, zx, zy;
  mpz_init (q);
  mpz_init (r);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
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
scm_integer_centered_quotient_ii (scm_t_inum x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("centered-quotient");

  scm_t_inum q = x / y;
  scm_t_inum r = x % y;
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
  return long_to_scm (q);
}

SCM
scm_integer_centered_quotient_iz (scm_t_inum x, SCM y)
{
  return integer_centered_quotient_zz (long_to_bignum (x),
                                       scm_bignum (y));
}

SCM
scm_integer_centered_quotient_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("centered-quotient");
  else if (y == 1)
    return x;
  else
    {
      mpz_t q, zx;
      mpz_init (q);
      alias_bignum_to_mpz (scm_bignum (x), zx);
      scm_t_inum r;
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
scm_integer_centered_quotient_zz (SCM x, SCM y)
{
  return integer_centered_quotient_zz (scm_bignum (x), scm_bignum (y));
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
scm_integer_centered_remainder_ii (scm_t_inum x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("centered-remainder");

  scm_t_inum r = x % y;
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
scm_integer_centered_remainder_iz (scm_t_inum x, SCM y)
{
  return integer_centered_remainder_zz (long_to_bignum (x),
                                        scm_bignum (y));
}

SCM
scm_integer_centered_remainder_zi (SCM x, scm_t_inum y)
{
  mpz_t zx;
  alias_bignum_to_mpz (scm_bignum (x), zx);

  if (y == 0)
    scm_num_overflow ("centered-remainder");

  scm_t_inum r;
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
scm_integer_centered_remainder_zz (SCM x, SCM y)
{
  return integer_centered_remainder_zz (scm_bignum (x), scm_bignum (y));
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
scm_integer_centered_divide_ii (scm_t_inum x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("centered-divide");

  scm_t_inum q = x / y;
  scm_t_inum r = x % y;
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
  *qp = long_to_scm (q);
  *rp = SCM_I_MAKINUM (r);
}

void
scm_integer_centered_divide_iz (scm_t_inum x, SCM y, SCM *qp, SCM *rp)
{
  integer_centered_divide_zz (long_to_bignum (x), scm_bignum (y), qp, rp);
}

void
scm_integer_centered_divide_zi (SCM x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("centered-divide");

  mpz_t q, zx;
  mpz_init (q);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  scm_t_inum r;

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
scm_integer_centered_divide_zz (SCM x, SCM y, SCM *qp, SCM *rp)
{
  integer_centered_divide_zz (scm_bignum (x), scm_bignum (y), qp, rp);
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
scm_integer_round_quotient_ii (scm_t_inum x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("round-quotient");

  scm_t_inum q = x / y;
  scm_t_inum r = x % y;
  scm_t_inum ay = y;
  scm_t_inum r2 = 2 * r;

  if (y < 0)
    {
      ay = -ay;
      r2 = -r2;
    }

  if (q & 1L)
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
  return long_to_scm (q);
}

SCM
scm_integer_round_quotient_iz (scm_t_inum x, SCM y)
{
  return integer_round_quotient_zz (long_to_bignum (x), scm_bignum (y));
}

SCM
scm_integer_round_quotient_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("round-quotient");
  if (y == 1)
    return x;

  mpz_t q, zx;
  mpz_init (q);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  scm_t_inum r;
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
scm_integer_round_quotient_zz (SCM x, SCM y)
{
  SCM q, r, r2;
  int cmp, needs_adjustment;

  /* Note that x might be small enough to fit into a
     fixnum, so we must not let it escape into the wild */
  q = scm_i_mkbig ();
  r = scm_i_mkbig ();
  r2 = scm_i_mkbig ();

  mpz_fdiv_qr (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (r),
	       SCM_I_BIG_MPZ (x), SCM_I_BIG_MPZ (y));
  mpz_mul_2exp (SCM_I_BIG_MPZ (r2), SCM_I_BIG_MPZ (r), 1);  /* r2 = 2*r */
  scm_remember_upto_here_2 (x, r);

  cmp = mpz_cmpabs (SCM_I_BIG_MPZ (r2), SCM_I_BIG_MPZ (y));
  if (mpz_odd_p (SCM_I_BIG_MPZ (q)))
    needs_adjustment = (cmp >= 0);
  else
    needs_adjustment = (cmp > 0);
  scm_remember_upto_here_2 (r2, y);

  if (needs_adjustment)
    mpz_add_ui (SCM_I_BIG_MPZ (q), SCM_I_BIG_MPZ (q), 1);

  return scm_i_normbig (q);
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
scm_integer_round_remainder_ii (scm_t_inum x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("round-remainder");

  scm_t_inum q = x / y;
  scm_t_inum r = x % y;
  scm_t_inum ay = y;
  scm_t_inum r2 = 2 * r;

  if (y < 0)
    {
      ay = -ay;
      r2 = -r2;
    }

  if (q & 1L)
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
scm_integer_round_remainder_iz (scm_t_inum x, SCM y)
{
  return integer_round_remainder_zz (long_to_bignum (x), scm_bignum (y));
}

SCM
scm_integer_round_remainder_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    scm_num_overflow ("round-remainder");

  mpz_t q, zx;
  scm_t_inum r;
  int needs_adjustment;

  mpz_init (q);
  alias_bignum_to_mpz (scm_bignum (x), zx);

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
scm_integer_round_remainder_zz (SCM x, SCM y)
{
  return integer_round_remainder_zz (scm_bignum (x), scm_bignum (y));
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
scm_integer_round_divide_ii (scm_t_inum x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("round-divide");

  scm_t_inum q = x / y;
  scm_t_inum r = x % y;
  scm_t_inum ay = y;
  scm_t_inum r2 = 2 * r;

  if (y < 0)
    {
      ay = -ay;
      r2 = -r2;
    }

  if (q & 1L)
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
  *qp = long_to_scm (q);
  *rp = SCM_I_MAKINUM (r);
}

void
scm_integer_round_divide_iz (scm_t_inum x, SCM y, SCM *qp, SCM *rp)
{
  integer_round_divide_zz (long_to_bignum (x), scm_bignum (y), qp, rp);
}

void
scm_integer_round_divide_zi (SCM x, scm_t_inum y, SCM *qp, SCM *rp)
{
  if (y == 0)
    scm_num_overflow ("round-divide");

  mpz_t q, zx;
  mpz_init (q);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  scm_t_inum r;
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
scm_integer_round_divide_zz (SCM x, SCM y, SCM *qp, SCM *rp)
{
  integer_round_divide_zz (scm_bignum (x), scm_bignum (y), qp, rp);
}

SCM
scm_integer_gcd_ii (scm_t_inum x, scm_t_inum y)
{
  scm_t_inum u = x < 0 ? -x : x;
  scm_t_inum v = y < 0 ? -y : y;
  scm_t_inum result;
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
  return ulong_to_scm (result);
}

SCM
scm_integer_gcd_zi (SCM x, scm_t_inum y)
{
  scm_t_bits result;
  if (y == 0)
    return scm_abs (x);
  if (y < 0)
    y = -y;
  result = mpz_gcd_ui (NULL, SCM_I_BIG_MPZ (x), y);
  scm_remember_upto_here_1 (x);
  return ulong_to_scm (result);
}

SCM
scm_integer_gcd_zz (SCM x, SCM y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_gcd (result, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (result);
}

SCM
scm_integer_lcm_ii (scm_t_inum x, scm_t_inum y)
{
  SCM d = scm_integer_gcd_ii (x, y);
  if (scm_is_eq (d, SCM_INUM0))
    return d;
  else
    return scm_abs (scm_product (SCM_I_MAKINUM (x),
                                 scm_quotient (SCM_I_MAKINUM (y), d)));
}

SCM
scm_integer_lcm_zi (SCM x, scm_t_inum y)
{
  if (y == 0) return SCM_INUM0;
  if (y < 0) y = - y;
  mpz_t result, zx;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  mpz_lcm_ui (result, zx, y);
  scm_remember_upto_here_1 (x);
  return take_mpz (result);
}

SCM
scm_integer_lcm_zz (SCM x, SCM y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
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
scm_integer_logand_ii (scm_t_inum x, scm_t_inum y)
{
  return SCM_I_MAKINUM (x & y);
}

SCM
scm_integer_logand_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    return SCM_INUM0;

  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  mpz_init_set_si (zy, y);
  mpz_and (result, zy, zx);
  scm_remember_upto_here_1 (x);
  mpz_clear (zy);
  return take_mpz (result);
}

SCM
scm_integer_logand_zz (SCM x, SCM y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_and (result, zx, zy);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (result);
}

SCM
scm_integer_logior_ii (scm_t_inum x, scm_t_inum y)
{
  return SCM_I_MAKINUM (x | y);
}

SCM
scm_integer_logior_zi (SCM x, scm_t_inum y)
{
  if (y == 0)
    return x;

  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  mpz_init_set_si (zy, y);
  mpz_ior (result, zy, zx);
  scm_remember_upto_here_1 (x);
  mpz_clear (zy);
  return take_mpz (result);
}

SCM
scm_integer_logior_zz (SCM x, SCM y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_ior (result, zy, zx);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (result);
}

SCM
scm_integer_logxor_ii (scm_t_inum x, scm_t_inum y)
{
  return SCM_I_MAKINUM (x ^ y);
}

SCM
scm_integer_logxor_zi (SCM x, scm_t_inum y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  mpz_init_set_si (zy, y);
  mpz_xor (result, zy, zx);
  scm_remember_upto_here_1 (x);
  mpz_clear (zy);
  return take_mpz (result);
}

SCM
scm_integer_logxor_zz (SCM x, SCM y)
{
  mpz_t result, zx, zy;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (x), zx);
  alias_bignum_to_mpz (scm_bignum (y), zy);
  mpz_xor (result, zy, zx);
  scm_remember_upto_here_2 (x, y);
  return take_mpz (result);
}

int
scm_integer_logtest_ii (scm_t_inum x, scm_t_inum y)
{
  return (x & y) ? 1 : 0;
}

int
scm_integer_logtest_zi (SCM x, scm_t_inum y)
{
  return scm_is_eq (scm_integer_logand_zi (x, y), SCM_INUM0);
}

int
scm_integer_logtest_zz (SCM x, SCM y)
{
  return scm_is_eq (scm_integer_logand_zz (x, y), SCM_INUM0);
}

int
scm_integer_logbit_ui (unsigned long index, scm_t_inum n)
{
  if (index < SCM_LONG_BIT)
    /* Assume two's complement representation.  */
    return (n >> index) & 1;
  else
    return n < 0;
}

int
scm_integer_logbit_uz (unsigned long index, SCM n)
{
  mpz_t zn;
  alias_bignum_to_mpz (scm_bignum (n), zn);
  int val = mpz_tstbit (zn, index);
  scm_remember_upto_here_1 (n);
  return val;
}

SCM
scm_integer_lognot_i (scm_t_inum n)
{
  return SCM_I_MAKINUM (~n);
}

SCM
scm_integer_lognot_z (SCM n)
{
  mpz_t result, zn;
  mpz_init (result);
  alias_bignum_to_mpz (scm_bignum (n), zn);
  mpz_com (result, zn);
  scm_remember_upto_here_1 (n);
  return take_mpz (result);
}
