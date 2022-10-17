/* srfi-60.c --- Integers as Bits

   Copyright 2005-2006,2008,2010,2014,2018,2021,2022
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

#include "boolean.h"
#include "eq.h"
#include "extensions.h"
#include "gsubr.h"
#include "integers.h"
#include "list.h"
#include "numbers.h"
#include "pairs.h"
#include "version.h"

#include "srfi-60.h"


SCM_DEFINE (scm_srfi60_log2_binary_factors, "log2-binary-factors", 1, 0, 0,
            (SCM n),
	    "Return a count of how many factors of 2 are present in @var{n}.\n"
	    "This is also the bit index of the lowest 1 bit in @var{n}.  If\n"
	    "@var{n} is 0, the return is @math{-1}.\n"
	    "\n"
	    "@example\n"
	    "(log2-binary-factors 6) @result{} 1\n"
	    "(log2-binary-factors -8) @result{} 3\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_log2_binary_factors
{
  SCM ret = SCM_EOL;

  if (SCM_I_INUMP (n))
    return scm_integer_scan1_i (SCM_I_INUM (n));
  else if (SCM_BIGP (n))
    return scm_integer_scan1_z (scm_bignum (n));
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);

  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_copy_bit, "copy-bit", 3, 0, 0,
            (SCM index, SCM n, SCM newbit),
	    "Return @var{n} with the bit at @var{index} set according to\n"
	    "@var{newbit}.  @var{newbit} should be @code{#t} to set the bit\n"
	    "to 1, or @code{#f} to set it to 0.  Bits other than at\n"
	    "@var{index} are unchanged in the return.\n"
	    "\n"
	    "@example\n"
	    "(copy-bit 1 #b0101 #t) @result{} 7\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_copy_bit
{
  uintptr_t ii;
  int bb;

  ii = scm_to_uintptr_t (index);
  bb = scm_to_bool (newbit);

  if (SCM_I_INUMP (n))
    {
      if (scm_integer_logbit_ui (ii, SCM_I_INUM (n)) == bb)
        return n;
    }
  else if (SCM_BIGP (n))
    {
      if (scm_integer_logbit_uz (ii, scm_bignum (n)) == bb)
        return n;
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);

  return scm_logxor (n, ii == 0 ? SCM_INUM1 : scm_integer_lsh_iu (1, ii));
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_rotate_bit_field, "rotate-bit-field", 4, 0, 0,
            (SCM n, SCM count, SCM start, SCM end),
	    "Return @var{n} with the bit field from @var{start} (inclusive)\n"
	    "to @var{end} (exclusive) rotated upwards by @var{count} bits.\n"
	    "\n"
	    "@var{count} can be positive or negative, and it can be more\n"
	    "than the field width (it'll be reduced modulo the width).\n"
	    "\n"
	    "@example\n"
	    "(rotate-bit-field #b0110 2 1 4) @result{} #b1010\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_rotate_bit_field
{
  uintptr_t ss = scm_to_uintptr_t (start);
  uintptr_t ee = scm_to_uintptr_t (end);
  uintptr_t ww, cc;

  SCM_ASSERT_RANGE (3, end, (ee >= ss));
  ww = ee - ss;

  /* we must avoid division by zero, and a field whose width is 0 or 1
     will be left unchanged anyway, so in that case we set cc to 0. */
  if (ww <= 1)
    cc = 0;
  else
    cc = scm_to_uintptr_t (scm_modulo (count, scm_difference (end, start)));

  mpz_t zn;

  if (SCM_I_INUMP (n))
    {
      intptr_t nn = SCM_I_INUM (n);

      if (ee <= SCM_INTPTR_T_BIT-1)
        {
          /* Everything fits within a intptr_t.  To avoid undefined
             behavior when shifting negative numbers, we do all
             operations using unsigned values, and then convert to
             signed at the end. */
          uintptr_t unn = nn;
          uintptr_t below = unn &  ((1UL << ss) - 1);  /* below start */
          uintptr_t above = unn & ~((1UL << ee) - 1);  /* above end */
          uintptr_t fmask = ((1UL << ww) - 1) << ss;   /* field mask */
          uintptr_t ff = unn & fmask;                  /* field */
          uintptr_t uresult = (above
                                   | ((ff << cc) & fmask)
                                   | ((ff >> (ww-cc)) & fmask)
                                   | below);
          intptr_t result;

          if (uresult > INTPTR_MAX)
            /* The high bit is set in uresult, so the result is
               negative.  We have to handle the conversion to signed
               integer carefully, to avoid undefined behavior.  First we
               compute ~uresult, equivalent to (ULONG_MAX - uresult),
               which will be between 0 and LONG_MAX (inclusive): exactly
               the set of numbers that can be represented as both intptr_t
               and uintptr_p and thus convertible between them.  We
               cast that difference to a signed long and then substract
               it from -1. */
            result = -1 - (intptr_t) ~uresult;
          else
            result = (intptr_t) uresult;

          return scm_from_intptr_t (result);
        }
      else
        {
          /* if there's no movement, avoid creating a bignum. */
          if (cc == 0)
            return n;

          mpz_init_set_si (zn, nn);
        }
    }
  else if (SCM_BIGP (n))
    {
      /* if there's no movement, avoid creating a new bignum. */
      if (cc == 0)
        return n;
      scm_integer_init_set_mpz_z (scm_bignum (n), zn);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);

  mpz_t tmp, r;

  mpz_init (tmp);
  mpz_init_set_si (r, 0);

  /* portion above end */
  mpz_fdiv_q_2exp (r, zn, ee);
  mpz_mul_2exp (r, r, ee);

  /* field high part, width-count bits from start go to start+count */
  mpz_fdiv_q_2exp (tmp, zn, ss);
  mpz_fdiv_r_2exp (tmp, tmp, ww - cc);
  mpz_mul_2exp (tmp, tmp, ss + cc);
  mpz_ior (r, r, tmp);

  /* field low part, count bits from end-count go to start */
  mpz_fdiv_q_2exp (tmp, zn, ee - cc);
  mpz_fdiv_r_2exp (tmp, tmp, cc);
  mpz_mul_2exp (tmp, tmp, ss);
  mpz_ior (r, r, tmp);

  /* portion below start */
  mpz_fdiv_r_2exp (tmp, zn, ss);
  mpz_ior (r, r, tmp);

  mpz_clear (zn);
  mpz_clear (tmp);

  /* bits moved around might leave us in range of an inum */
  SCM ret = scm_from_mpz (r);
  mpz_clear (r);
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_reverse_bit_field, "reverse-bit-field", 3, 0, 0,
            (SCM n, SCM start, SCM end),
	    "Return @var{n} with the bits between @var{start} (inclusive) to\n"
	    "@var{end} (exclusive) reversed.\n"
	    "\n"
	    "@example\n"
	    "(reverse-bit-field #b101001 2 4) @result{} #b100101\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_reverse_bit_field
{
  intptr_t ss = scm_to_intptr_t (start);
  intptr_t ee = scm_to_intptr_t (end);
  intptr_t swaps = (ee - ss) / 2;  /* number of swaps */
  mpz_t b;

  if (SCM_I_INUMP (n))
    {
      intptr_t nn = SCM_I_INUM (n);

      if (ee <= SCM_INTPTR_T_BIT-1)
        {
          /* all within a intptr_t */
          intptr_t smask = 1L << ss;
          intptr_t emask = 1L << (ee-1);
          for ( ; swaps > 0; swaps--)
            {
              intptr_t sbit = nn & smask;
              intptr_t ebit = nn & emask;
              nn ^= sbit ^ (ebit ? smask : 0)  /* zap sbit, put ebit value */
                ^   ebit ^ (sbit ? emask : 0); /* zap ebit, put sbit value */

              smask <<= 1;
              emask >>= 1;
            }
          return scm_from_intptr_t (nn);
        }
      else
        {
          /* avoid creating a new bignum if reversing only 0 or 1 bits */
          if (ee - ss <= 1)
            return n;
          mpz_init_set_si (b, nn);
        }
    }
  else if (SCM_BIGP (n))
    {
      /* avoid creating a new bignum if reversing only 0 or 1 bits */
      if (ee - ss <= 1)
        return n;
      scm_integer_init_set_mpz_z (scm_bignum (n), b);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);

  ee--;
  for ( ; swaps > 0; swaps--)
    {
      int sbit = mpz_tstbit (b, ss);
      int ebit = mpz_tstbit (b, ee);
      if (sbit ^ ebit)
        {
          /* the two bits are different, flip them */
          if (sbit)
            {
              mpz_clrbit (b, ss);
              mpz_setbit (b, ee);
            }
          else
            {
              mpz_setbit (b, ss);
              mpz_clrbit (b, ee);
            }
        }
      ss++;
      ee--;
    }
  SCM ret = scm_integer_from_mpz (b);
  mpz_clear (b);
  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_integer_to_list, "integer->list", 1, 1, 0,
            (SCM n, SCM len),
	    "Return bits from @var{n} in the form of a list of @code{#t} for\n"
	    "1 and @code{#f} for 0.  The least significant @var{len} bits\n"
	    "are returned, and the first list element is the most\n"
	    "significant of those bits.  If @var{len} is not given, the\n"
	    "default is @code{(integer-length @var{n})} (@pxref{Bitwise\n"
	    "Operations}).\n"
	    "\n"
	    "@example\n"
	    "(integer->list 6)   @result{} (#t #t #f)\n"
	    "(integer->list 1 4) @result{} (#f #f #f #t)\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_integer_to_list
{
  SCM ret = SCM_EOL;
  uintptr_t ll;

  if (SCM_UNBNDP (len))
    len = scm_integer_length (n);
  ll = scm_to_uintptr_t (len);

  if (SCM_I_INUMP (n))
    {
      intptr_t nn = SCM_I_INUM (n);
      for (uintptr_t i = 0; i < ll; i++)
        ret = scm_cons (scm_from_bool (scm_integer_logbit_ui (i, nn)), ret);
    }
  else if (SCM_BIGP (n))
    {
      struct scm_bignum *nn = scm_bignum (n);
      for (uintptr_t i = 0; i < ll; i++)
        ret = scm_cons (scm_from_bool (scm_integer_logbit_uz (i, nn)), ret);
    }
  else
    SCM_WRONG_TYPE_ARG (SCM_ARG1, n);

  return ret;
}
#undef FUNC_NAME


SCM_DEFINE (scm_srfi60_list_to_integer, "list->integer", 1, 0, 0,
            (SCM lst),
	    "Return an integer formed bitwise from the given @var{lst} list\n"
	    "of booleans.  Each boolean is @code{#t} for a 1 and @code{#f}\n"
	    "for a 0.  The first element becomes the most significant bit in\n"
	    "the return.\n"
	    "\n"
	    "@example\n"
	    "(list->integer '(#t #f #t #f)) @result{} 10\n"
	    "@end example")
#define FUNC_NAME s_scm_srfi60_list_to_integer
{
  intptr_t len;

  /* strip high zero bits from lst; after this the length tells us whether
     an inum or bignum is required */
  while (scm_is_pair (lst) && scm_is_false (SCM_CAR (lst)))
    lst = SCM_CDR (lst);

  SCM_VALIDATE_LIST_COPYLEN (SCM_ARG1, lst, len);

  if (len <= SCM_I_FIXNUM_BIT - 1)
    {
      /* fits an inum (a positive inum) */
      intptr_t n = 0;
      while (scm_is_pair (lst))
        {
          n <<= 1;
          if (! scm_is_false (SCM_CAR (lst)))
            n++;
          lst = SCM_CDR (lst);
        }
      return SCM_I_MAKINUM (n);
    }
  else
    {
      mpz_t z;
      mpz_init (z);
      while (scm_is_pair (lst))
        {
          len--;
          if (! scm_is_false (SCM_CAR (lst)))
            mpz_setbit (z, len);
          lst = SCM_CDR (lst);
        }
      SCM ret = scm_from_mpz (z);
      mpz_clear (z);
      return ret;
    }
}
#undef FUNC_NAME


/* note: don't put "scm_srfi60_list_to_integer" arg on its own line, a
   newline breaks the snarfer */
SCM_REGISTER_PROC (s_srfi60_booleans_to_integer, "booleans->integer", 0, 0, 1, scm_srfi60_list_to_integer);


void
scm_register_srfi_60 (void)
{
  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_srfi_60",
                            (scm_t_extension_init_func)scm_init_srfi_60, NULL);
}

void
scm_init_srfi_60 (void)
{
#ifndef SCM_MAGIC_SNARFER
#include "srfi-60.x"
#endif
}
