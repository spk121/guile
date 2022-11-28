#ifndef SCM_INTEGERS_H
#define SCM_INTEGERS_H

/* Copyright 2021, 2022 Free Software Foundation, Inc.

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



#include "libguile/numbers.h"

struct scm_bignum;

static inline struct scm_bignum *
scm_bignum (SCM x)
{
  if (!SCM_BIGP (x)) abort ();
  return (struct scm_bignum *) SCM_UNPACK (x);
}

SCM_INTERNAL SCM scm_integer_from_mpz (const mpz_t n);
SCM_INTERNAL void scm_integer_set_mpz_z (struct scm_bignum *z, mpz_t n);
SCM_INTERNAL void scm_integer_init_set_mpz_z (struct scm_bignum *z, mpz_t n);

SCM_INTERNAL int scm_is_integer_odd_i (intptr_t i);
SCM_INTERNAL int scm_is_integer_odd_z (struct scm_bignum *z);

SCM_INTERNAL SCM scm_integer_abs_i (intptr_t i);
SCM_INTERNAL SCM scm_integer_abs_z (struct scm_bignum *z);

#define DECLARE_QUOTIENT_OPERATORS(stem)                                \
  SCM_INTERNAL SCM scm_integer_##stem##_quotient_ii (intptr_t x,      \
                                                     intptr_t y);     \
  SCM_INTERNAL SCM scm_integer_##stem##_quotient_iz (intptr_t x,      \
                                                     struct scm_bignum *y); \
  SCM_INTERNAL SCM scm_integer_##stem##_quotient_zi (struct scm_bignum *x, \
                                                     intptr_t y);     \
  SCM_INTERNAL SCM scm_integer_##stem##_quotient_zz (struct scm_bignum *x, \
                                                     struct scm_bignum *y);

#define DECLARE_REMAINDER_OPERATORS(stem)                               \
  SCM_INTERNAL SCM scm_integer_##stem##_remainder_ii (intptr_t x,     \
                                                      intptr_t y);    \
  SCM_INTERNAL SCM scm_integer_##stem##_remainder_iz (intptr_t x,     \
                                                      struct scm_bignum *y); \
  SCM_INTERNAL SCM scm_integer_##stem##_remainder_zi (struct scm_bignum *x, \
                                                      intptr_t y);    \
  SCM_INTERNAL SCM scm_integer_##stem##_remainder_zz (struct scm_bignum *x, \
                                                      struct scm_bignum *y);

#define DECLARE_DIVIDE_OPERATORS(stem)                                  \
  SCM_INTERNAL void scm_integer_##stem##_divide_ii (intptr_t x,       \
                                                    intptr_t y,       \
                                                    SCM *qp, SCM *rp);  \
  SCM_INTERNAL void scm_integer_##stem##_divide_iz (intptr_t x,       \
                                                    struct scm_bignum *y, \
                                                    SCM *qp, SCM *rp);  \
  SCM_INTERNAL void scm_integer_##stem##_divide_zi (struct scm_bignum *x, \
                                                    intptr_t y,       \
                                                    SCM *qp, SCM *rp);  \
  SCM_INTERNAL void scm_integer_##stem##_divide_zz (struct scm_bignum *x, \
                                                    struct scm_bignum *y, \
                                                    SCM *qp, SCM *rp);

#define DECLARE_DIVISION_OPERATORS(stem)                                \
  DECLARE_QUOTIENT_OPERATORS(stem);                                     \
  DECLARE_REMAINDER_OPERATORS(stem);                                    \
  DECLARE_DIVIDE_OPERATORS(stem)

DECLARE_DIVISION_OPERATORS(floor);
DECLARE_DIVISION_OPERATORS(ceiling);
DECLARE_DIVISION_OPERATORS(truncate);
DECLARE_DIVISION_OPERATORS(centered);
DECLARE_DIVISION_OPERATORS(round);

SCM_INTERNAL SCM scm_integer_gcd_ii (intptr_t x, intptr_t y);
SCM_INTERNAL SCM scm_integer_gcd_zi (struct scm_bignum *x, intptr_t y);
SCM_INTERNAL SCM scm_integer_gcd_zz (struct scm_bignum *x,
                                     struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_lcm_ii (intptr_t x, intptr_t y);
SCM_INTERNAL SCM scm_integer_lcm_zi (struct scm_bignum *x, intptr_t y);
SCM_INTERNAL SCM scm_integer_lcm_zz (struct scm_bignum *x,
                                     struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_logand_ii (intptr_t x, intptr_t y);
SCM_INTERNAL SCM scm_integer_logand_zi (struct scm_bignum *x, intptr_t y);
SCM_INTERNAL SCM scm_integer_logand_zz (struct scm_bignum *x,
                                        struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_logior_ii (intptr_t x, intptr_t y);
SCM_INTERNAL SCM scm_integer_logior_zi (struct scm_bignum *x, intptr_t y);
SCM_INTERNAL SCM scm_integer_logior_zz (struct scm_bignum *x,
                                        struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_logxor_ii (intptr_t x, intptr_t y);
SCM_INTERNAL SCM scm_integer_logxor_zi (struct scm_bignum *x, intptr_t y);
SCM_INTERNAL SCM scm_integer_logxor_zz (struct scm_bignum *x,
                                        struct scm_bignum *y);

SCM_INTERNAL int scm_integer_logtest_ii (intptr_t x, intptr_t y);
SCM_INTERNAL int scm_integer_logtest_zi (struct scm_bignum *x, intptr_t y);
SCM_INTERNAL int scm_integer_logtest_zz (struct scm_bignum *x,
                                         struct scm_bignum *y);

SCM_INTERNAL int scm_integer_logbit_ui (uintptr_t bit, intptr_t n);
SCM_INTERNAL int scm_integer_logbit_uz (uintptr_t bit,
                                        struct scm_bignum *n);

SCM_INTERNAL SCM scm_integer_lognot_i (intptr_t n);
SCM_INTERNAL SCM scm_integer_lognot_z (struct scm_bignum *n);

SCM_INTERNAL SCM scm_integer_expt_ii (intptr_t n, intptr_t k);
SCM_INTERNAL SCM scm_integer_expt_zi (struct scm_bignum *n, intptr_t k);

SCM_INTERNAL SCM scm_integer_modulo_expt_nnn (SCM n, SCM k, SCM m);

SCM_INTERNAL SCM scm_integer_lsh_iu (intptr_t n, uintptr_t count);
SCM_INTERNAL SCM scm_integer_lsh_zu (struct scm_bignum *n,
                                     uintptr_t count);
SCM_INTERNAL SCM scm_integer_floor_rsh_iu (intptr_t n, uintptr_t count);
SCM_INTERNAL SCM scm_integer_floor_rsh_zu (struct scm_bignum *n,
                                           uintptr_t count);
SCM_INTERNAL SCM scm_integer_round_rsh_iu (intptr_t n, uintptr_t count);
SCM_INTERNAL SCM scm_integer_round_rsh_zu (struct scm_bignum *n,
                                           uintptr_t count);

SCM_INTERNAL SCM scm_integer_bit_extract_i (intptr_t n, uintptr_t start,
                                            uintptr_t bits);
SCM_INTERNAL SCM scm_integer_bit_extract_z (struct scm_bignum *n,
                                            uintptr_t start,
                                            uintptr_t bits);

SCM_INTERNAL SCM scm_integer_logcount_i (intptr_t n);
SCM_INTERNAL SCM scm_integer_logcount_z (struct scm_bignum *n);

SCM_INTERNAL SCM scm_integer_length_i (intptr_t n);
SCM_INTERNAL SCM scm_integer_length_z (struct scm_bignum *n);

SCM_INTERNAL SCM scm_integer_to_string_i (intptr_t n, int base);
SCM_INTERNAL SCM scm_integer_to_string_z (struct scm_bignum *n, int base);

SCM_INTERNAL int scm_is_integer_equal_ir (intptr_t x, double y);
SCM_INTERNAL int scm_is_integer_equal_ic (intptr_t x,
                                          double real, double imag);
SCM_INTERNAL int scm_is_integer_equal_zz (struct scm_bignum *x,
                                          struct scm_bignum *y);
SCM_INTERNAL int scm_is_integer_equal_zr (struct scm_bignum *x, double y);
SCM_INTERNAL int scm_is_integer_equal_zc (struct scm_bignum *x,
                                          double real, double imag);

SCM_INTERNAL int scm_is_integer_less_than_ir (intptr_t x, double y);
SCM_INTERNAL int scm_is_integer_less_than_ri (double x, intptr_t y);
SCM_INTERNAL int scm_is_integer_less_than_zz (struct scm_bignum *x,
                                              struct scm_bignum *y);
SCM_INTERNAL int scm_is_integer_less_than_zr (struct scm_bignum *x, double y);
SCM_INTERNAL int scm_is_integer_less_than_rz (double y, struct scm_bignum *x);

SCM_INTERNAL int scm_is_integer_positive_z (struct scm_bignum *x);
SCM_INTERNAL int scm_is_integer_negative_z (struct scm_bignum *x);

SCM_INTERNAL double scm_integer_frexp_z (struct scm_bignum *x, intptr_t *exp);
SCM_INTERNAL double scm_integer_to_double_z (struct scm_bignum *x);
SCM_INTERNAL SCM scm_integer_from_double (double val);

SCM_INTERNAL SCM scm_integer_add_ii (intptr_t x, intptr_t y);
SCM_INTERNAL SCM scm_integer_add_zi (struct scm_bignum *x, intptr_t y);
SCM_INTERNAL SCM scm_integer_add_zz (struct scm_bignum *x, struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_negate_i (intptr_t x);
SCM_INTERNAL SCM scm_integer_negate_z (struct scm_bignum *x);

SCM_INTERNAL SCM scm_integer_sub_ii (intptr_t x, intptr_t y);
SCM_INTERNAL SCM scm_integer_sub_iz (intptr_t x, struct scm_bignum *y);
SCM_INTERNAL SCM scm_integer_sub_zi (struct scm_bignum *x, intptr_t y);
SCM_INTERNAL SCM scm_integer_sub_zz (struct scm_bignum *x, struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_mul_ii (intptr_t x, intptr_t y);
SCM_INTERNAL SCM scm_integer_mul_zi (struct scm_bignum *x, intptr_t y);
SCM_INTERNAL SCM scm_integer_mul_zz (struct scm_bignum *x, struct scm_bignum *y);

SCM_INTERNAL int scm_is_integer_divisible_ii (intptr_t x, intptr_t y);
SCM_INTERNAL int scm_is_integer_divisible_zi (struct scm_bignum *x,
                                              intptr_t y);
SCM_INTERNAL int scm_is_integer_divisible_zz (struct scm_bignum *x,
                                              struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_exact_quotient_ii (intptr_t n, intptr_t d);
SCM_INTERNAL SCM scm_integer_exact_quotient_iz (intptr_t n,
                                                struct scm_bignum *d);
SCM_INTERNAL SCM scm_integer_exact_quotient_zi (struct scm_bignum *n,
                                                intptr_t d);
SCM_INTERNAL SCM scm_integer_exact_quotient_zz (struct scm_bignum *n,
                                                struct scm_bignum *d);

#if SCM_SIZEOF_INTPTR_T == 4
SCM_INTERNAL SCM scm_integer_from_int32 (int32_t n);
SCM_INTERNAL SCM scm_integer_from_uint32 (uint32_t n);
SCM_INTERNAL int scm_integer_to_int32_z (struct scm_bignum *z, int32_t *val);
SCM_INTERNAL int scm_integer_to_uint32_z (struct scm_bignum *z, uint32_t *val);
#endif

SCM_INTERNAL int scm_integer_to_int64_z (struct scm_bignum *z, int64_t *val);
SCM_INTERNAL int scm_integer_to_uint64_z (struct scm_bignum *z, uint64_t *val);
SCM_INTERNAL SCM scm_integer_from_int64 (int64_t n);
SCM_INTERNAL SCM scm_integer_from_uint64 (uint64_t n);

SCM_INTERNAL void scm_integer_exact_sqrt_i (intptr_t k, SCM *s, SCM *r);
SCM_INTERNAL void scm_integer_exact_sqrt_z (struct scm_bignum *k,
                                            SCM *s, SCM *r);

SCM_INTERNAL int scm_is_integer_perfect_square_i (intptr_t k);
SCM_INTERNAL int scm_is_integer_perfect_square_z (struct scm_bignum *k);
SCM_INTERNAL SCM scm_integer_floor_sqrt_i (intptr_t k);
SCM_INTERNAL SCM scm_integer_floor_sqrt_z (struct scm_bignum *k);
SCM_INTERNAL double scm_integer_inexact_sqrt_i (intptr_t k);
SCM_INTERNAL double scm_integer_inexact_sqrt_z (struct scm_bignum *k);

SCM_INTERNAL SCM scm_integer_scan1_i (intptr_t n);
SCM_INTERNAL SCM scm_integer_scan1_z (struct scm_bignum *n);



#endif  /* SCM_INTEGERS_H */
