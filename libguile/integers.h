#ifndef SCM_INTEGERS_H
#define SCM_INTEGERS_H

/* Copyright 2021 Free Software Foundation, Inc.

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

SCM_INTERNAL int scm_is_integer_odd_i (scm_t_inum i);
SCM_INTERNAL int scm_is_integer_odd_z (SCM z);

SCM_INTERNAL SCM scm_integer_abs_i (scm_t_inum i);
SCM_INTERNAL SCM scm_integer_abs_z (SCM z);

SCM_INTERNAL SCM scm_integer_floor_quotient_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_floor_quotient_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_floor_quotient_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_floor_quotient_zz (SCM x, SCM y);

SCM_INTERNAL SCM scm_integer_floor_remainder_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_floor_remainder_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_floor_remainder_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_floor_remainder_zz (SCM x, SCM y);

SCM_INTERNAL void scm_integer_floor_divide_ii (scm_t_inum x, scm_t_inum y,
                                               SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_floor_divide_iz (scm_t_inum x, SCM y,
                                               SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_floor_divide_zi (SCM x, scm_t_inum y,
                                               SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_floor_divide_zz (SCM x, SCM y,
                                               SCM *qp, SCM *rp);

SCM_INTERNAL SCM scm_integer_ceiling_quotient_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_ceiling_quotient_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_ceiling_quotient_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_ceiling_quotient_zz (SCM x, SCM y);

SCM_INTERNAL SCM scm_integer_ceiling_remainder_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_ceiling_remainder_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_ceiling_remainder_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_ceiling_remainder_zz (SCM x, SCM y);

SCM_INTERNAL void scm_integer_ceiling_divide_ii (scm_t_inum x, scm_t_inum y,
                                                 SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_ceiling_divide_iz (scm_t_inum x, SCM y,
                                                 SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_ceiling_divide_zi (SCM x, scm_t_inum y,
                                                 SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_ceiling_divide_zz (SCM x, SCM y,
                                                 SCM *qp, SCM *rp);

SCM_INTERNAL SCM scm_integer_truncate_quotient_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_truncate_quotient_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_truncate_quotient_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_truncate_quotient_zz (SCM x, SCM y);

SCM_INTERNAL SCM scm_integer_truncate_remainder_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_truncate_remainder_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_truncate_remainder_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_truncate_remainder_zz (SCM x, SCM y);

SCM_INTERNAL void scm_integer_truncate_divide_ii (scm_t_inum x, scm_t_inum y,
                                                  SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_truncate_divide_iz (scm_t_inum x, SCM y,
                                                  SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_truncate_divide_zi (SCM x, scm_t_inum y,
                                                  SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_truncate_divide_zz (SCM x, SCM y,
                                                  SCM *qp, SCM *rp);

SCM_INTERNAL SCM scm_integer_centered_quotient_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_centered_quotient_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_centered_quotient_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_centered_quotient_zz (SCM x, SCM y);

SCM_INTERNAL SCM scm_integer_centered_remainder_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_centered_remainder_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_centered_remainder_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_centered_remainder_zz (SCM x, SCM y);

SCM_INTERNAL void scm_integer_centered_divide_ii (scm_t_inum x, scm_t_inum y,
                                                  SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_centered_divide_iz (scm_t_inum x, SCM y,
                                                  SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_centered_divide_zi (SCM x, scm_t_inum y,
                                                  SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_centered_divide_zz (SCM x, SCM y,
                                                  SCM *qp, SCM *rp);

SCM_INTERNAL SCM scm_integer_round_quotient_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_round_quotient_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_round_quotient_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_round_quotient_zz (SCM x, SCM y);

SCM_INTERNAL SCM scm_integer_round_remainder_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_round_remainder_iz (scm_t_inum x, SCM y);
SCM_INTERNAL SCM scm_integer_round_remainder_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_round_remainder_zz (SCM x, SCM y);

SCM_INTERNAL void scm_integer_round_divide_ii (scm_t_inum x, scm_t_inum y,
                                               SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_round_divide_iz (scm_t_inum x, SCM y,
                                               SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_round_divide_zi (SCM x, scm_t_inum y,
                                               SCM *qp, SCM *rp);
SCM_INTERNAL void scm_integer_round_divide_zz (SCM x, SCM y,
                                               SCM *qp, SCM *rp);

SCM_INTERNAL SCM scm_integer_gcd_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_gcd_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_gcd_zz (SCM x, SCM y);

SCM_INTERNAL SCM scm_integer_lcm_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_lcm_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_lcm_zz (SCM x, SCM y);

SCM_INTERNAL SCM scm_integer_logand_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logand_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logand_zz (SCM x, SCM y);

SCM_INTERNAL SCM scm_integer_logior_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logior_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logior_zz (SCM x, SCM y);

SCM_INTERNAL SCM scm_integer_logxor_ii (scm_t_inum x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logxor_zi (SCM x, scm_t_inum y);
SCM_INTERNAL SCM scm_integer_logxor_zz (SCM x, SCM y);



#endif  /* SCM_INTEGERS_H */
