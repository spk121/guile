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

SCM_INTERNAL int scm_is_integer_odd_i (scm_t_inum i);
SCM_INTERNAL int scm_is_integer_odd_z (struct scm_bignum *z);

SCM_INTERNAL SCM scm_integer_abs_i (scm_t_inum i);
SCM_INTERNAL SCM scm_integer_abs_z (struct scm_bignum *z);

#define DECLARE_QUOTIENT_OPERATORS(stem)                                \
  SCM_INTERNAL SCM scm_integer_##stem##_quotient_ii (scm_t_inum x,      \
                                                     scm_t_inum y);     \
  SCM_INTERNAL SCM scm_integer_##stem##_quotient_iz (scm_t_inum x,      \
                                                     struct scm_bignum *y); \
  SCM_INTERNAL SCM scm_integer_##stem##_quotient_zi (struct scm_bignum *x, \
                                                     scm_t_inum y);     \
  SCM_INTERNAL SCM scm_integer_##stem##_quotient_zz (struct scm_bignum *x, \
                                                     struct scm_bignum *y);

#define DECLARE_REMAINDER_OPERATORS(stem)                               \
  SCM_INTERNAL SCM scm_integer_##stem##_remainder_ii (scm_t_inum x,     \
                                                      scm_t_inum y);    \
  SCM_INTERNAL SCM scm_integer_##stem##_remainder_iz (scm_t_inum x,     \
                                                      struct scm_bignum *y); \
  SCM_INTERNAL SCM scm_integer_##stem##_remainder_zi (struct scm_bignum *x, \
                                                      scm_t_inum y);    \
  SCM_INTERNAL SCM scm_integer_##stem##_remainder_zz (struct scm_bignum *x, \
                                                      struct scm_bignum *y);

#define DECLARE_DIVIDE_OPERATORS(stem)                                  \
  SCM_INTERNAL void scm_integer_##stem##_divide_ii (scm_t_inum x,       \
                                                    scm_t_inum y,       \
                                                    SCM *qp, SCM *rp);  \
  SCM_INTERNAL void scm_integer_##stem##_divide_iz (scm_t_inum x,       \
                                                    struct scm_bignum *y, \
                                                    SCM *qp, SCM *rp);  \
  SCM_INTERNAL void scm_integer_##stem##_divide_zi (struct scm_bignum *x, \
                                                    scm_t_inum y,       \
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

SCM_INTERNAL SCM scm_integer_gcd_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_gcd_zi (struct scm_bignum *x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_gcd_zz (struct scm_bignum *x,
                                     struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_lcm_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_lcm_zi (struct scm_bignum *x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_lcm_zz (struct scm_bignum *x,
                                     struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_logand_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logand_zi (struct scm_bignum *x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logand_zz (struct scm_bignum *x,
                                        struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_logior_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logior_zi (struct scm_bignum *x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logior_zz (struct scm_bignum *x,
                                        struct scm_bignum *y);

SCM_INTERNAL SCM scm_integer_logxor_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logxor_zi (struct scm_bignum *x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logxor_zz (struct scm_bignum *x,
                                        struct scm_bignum *y);

SCM_INTERNAL int scm_integer_logtest_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL int scm_integer_logtest_zi (struct scm_bignum *x, scm_t_inum y);
SCM_INTERNAL int scm_integer_logtest_zz (struct scm_bignum *x,
                                         struct scm_bignum *y);

SCM_INTERNAL int scm_integer_logbit_ui (unsigned long bit, scm_t_inum n);
SCM_INTERNAL int scm_integer_logbit_uz (unsigned long bit,
                                        struct scm_bignum *n);

SCM_INTERNAL SCM scm_integer_lognot_i (scm_t_inum n);
SCM_INTERNAL SCM scm_integer_lognot_z (struct scm_bignum *n);

SCM_INTERNAL SCM scm_integer_modulo_expt_nnn (SCM n, SCM k, SCM m);

SCM_INTERNAL SCM scm_integer_lsh_iu (scm_t_inum n, unsigned long count);
SCM_INTERNAL SCM scm_integer_lsh_zu (struct scm_bignum *n,
                                     unsigned long count);
SCM_INTERNAL SCM scm_integer_floor_rsh_iu (scm_t_inum n, unsigned long count);
SCM_INTERNAL SCM scm_integer_floor_rsh_zu (struct scm_bignum *n,
                                           unsigned long count);
SCM_INTERNAL SCM scm_integer_round_rsh_iu (scm_t_inum n, unsigned long count);
SCM_INTERNAL SCM scm_integer_round_rsh_zu (struct scm_bignum *n,
                                           unsigned long count);

SCM_INTERNAL SCM scm_integer_bit_extract_i (scm_t_inum n, unsigned long start,
                                            unsigned long bits);
SCM_INTERNAL SCM scm_integer_bit_extract_z (struct scm_bignum *n,
                                            unsigned long start,
                                            unsigned long bits);

SCM_INTERNAL SCM scm_integer_logcount_i (scm_t_inum n);
SCM_INTERNAL SCM scm_integer_logcount_z (struct scm_bignum *n);

SCM_INTERNAL SCM scm_integer_length_i (scm_t_inum n);
SCM_INTERNAL SCM scm_integer_length_z (struct scm_bignum *n);

SCM_INTERNAL SCM scm_integer_to_string_i (scm_t_inum n, int base);
SCM_INTERNAL SCM scm_integer_to_string_z (struct scm_bignum *n, int base);

SCM_INTERNAL int scm_is_integer_equal_ir (scm_t_inum x, double y);
SCM_INTERNAL int scm_is_integer_equal_ic (scm_t_inum x,
                                          double real, double imag);
SCM_INTERNAL int scm_is_integer_equal_zz (struct scm_bignum *x,
                                          struct scm_bignum *y);
SCM_INTERNAL int scm_is_integer_equal_zr (struct scm_bignum *x, double y);
SCM_INTERNAL int scm_is_integer_equal_zc (struct scm_bignum *x,
                                          double real, double imag);

SCM_INTERNAL int scm_is_integer_less_than_ir (scm_t_inum x, double y);
SCM_INTERNAL int scm_is_integer_less_than_ri (double x, scm_t_inum y);
SCM_INTERNAL int scm_is_integer_less_than_zz (struct scm_bignum *x,
                                              struct scm_bignum *y);
SCM_INTERNAL int scm_is_integer_less_than_zr (struct scm_bignum *x, double y);
SCM_INTERNAL int scm_is_integer_less_than_rz (double y, struct scm_bignum *x);

SCM_INTERNAL int scm_is_integer_positive_z (struct scm_bignum *x);
SCM_INTERNAL int scm_is_integer_negative_z (struct scm_bignum *x);

SCM_INTERNAL double scm_integer_to_double_z (struct scm_bignum *x);

SCM_INTERNAL SCM scm_integer_add_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_add_zi (struct scm_bignum *x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_add_zz (struct scm_bignum *x, struct scm_bignum *y);



#endif  /* SCM_INTEGERS_H */
