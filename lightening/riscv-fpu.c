/*
 * Copyright (C) 2012-2024  Free Software Foundation, Inc.
 *
 * This file is part of GNU lightning.
 *
 * GNU lightning is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3, or (at your option)
 * any later version.
 *
 * GNU lightning is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * Authors:
 *      Paulo Cesar Pereira de Andrade
 *      Ekaitz Zarraga <ekaitz@elenq.tech>
 */

/*
 * RV32F Standard Extension
 */
#define _FLW(rd, rs1, im)              Itype(7, rd, 2, rs1, im)
#define _FSW(rs1, rs2, imm)            Stype(39, 2, rs1, rs2, imm)
#define _FMADD_S(rd, rs1, rs2, rs3)    R4type(67, rd, 0, rs1, rs2, 0, rs3)
#define _FMSUB_S(rd, rs1, rs2, rs3)    R4type(71, rd, 0, rs1, rs2, 0, rs3)
#define _FNMSUB_S(rd, rs1, rs2, rs3)   R4type(75, rd, 0, rs1, rs2, 0, rs3)
#define _FNMADD_S(rd, rs1, rs2, rs3)   R4type(79, rd, 0, rs1, rs2, 0, rs3)
#define _FADD_S(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 0)
#define _FSUB_S(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 4)
#define _FMUL_S(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 8)
#define _FDIV_S(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 12)
#define _FSQRT_S(rd, rs1)              Rtype(83, rd, 0, rs1, 0, 44)
#define _FSGNJ_S(rd, rs1, rs2)         Rtype(83, rd, 0, rs1, rs2, 16)
#define _FSGNJN_S(rd, rs1, rs2)        Rtype(83, rd, 1, rs1, rs2, 16)
#define _FSGNJX_S(rd, rs1, rs2)        Rtype(83, rd, 2, rs1, rs2, 16)
#define _FMIN_S(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 20)
#define _FMAX_S(rd, rs1, rs2)          Rtype(83, rd, 1, rs1, rs2, 20)
#define _FCVT_W_S(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 0, 96)
#define _FCVT_WU_S(rd, rs1, rm)        Rtype(83, rd, rm, rs1, 1, 96)
#define _FMV_X_W(rd, rs1)              Rtype(83, rd, 0, rs1, 0, 112)
#define _FEQ_S(rd, rs1, rs2)           Rtype(83, rd, 2, rs1, rs2, 80)
#define _FLT_S(rd, rs1, rs2)           Rtype(83, rd, 1, rs1, rs2, 80)
#define _FLE_S(rd, rs1, rs2)           Rtype(83, rd, 0, rs1, rs2, 80)
#define _FCLASS_S(rd, rs1)             Rtype(83, rd, 1, rs1, 0, 112)
#define _FCVT_S_W(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 0, 104)
#define _FCVT_S_WU(rd, rs1, rm)        Rtype(83, rd, rm, rs1, 1, 104)
#define _FMV_W_X(rd, rs1)              Rtype(83, rd, 0, rs1, 0, 120)
/*
 * RV64F Standard Extension (in addition to RV32F)
 */
#define _FCVT_L_S(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 2, 96)
#define _FCVT_LU_S(rd, rs1, rm)        Rtype(83, rd, rm, rs1, 3, 96)
#define _FCVT_S_L(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 2, 104)
#define _FCVT_S_LU(rd, rs1, rm)        Rtype(83, rd, rm, rs1, 3, 104)
/*
 * RV32D Standard Extension
 */
#define _FLD(rd, rs1, im)              Itype(7, rd, 3, rs1, im)
#define _FSD(rs1, rs2, imm)            Stype(39, 3, rs1, rs2, imm)
#define _FMADD_D(rd, rs1, rs2, rs3)    R4type(67, rd, 0, rs1, rs2, 1, rs3)
#define _FMSUB_D(rd, rs1, rs2, rs3)    R4type(71, rd, 0, rs1, rs2, 1, rs3)
#define _FNMSUB_D(rd, rs1, rs2, rs3)   R4type(75, rd, 0, rs1, rs2, 1, rs3)
#define _FNMADD_D(rd, rs1, rs2, rs3)   R4type(79, rd, 0, rs1, rs2, 1, rs3)
#define _FADD_D(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 1)
#define _FSUB_D(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 5)
#define _FMUL_D(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 9)
#define _FDIV_D(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 13)
#define _FSQRT_D(rd, rs1)              Rtype(83, rd, 0, rs1, 0, 45)
#define _FSGNJ_D(rd, rs1, rs2)         Rtype(83, rd, 0, rs1, rs2, 17)
#define _FSGNJN_D(rd, rs1, rs2)        Rtype(83, rd, 1, rs1, rs2, 17)
#define _FSGNJX_D(rd, rs1, rs2)        Rtype(83, rd, 2, rs1, rs2, 17)
#define _FMIN_D(rd, rs1, rs2)          Rtype(83, rd, 0, rs1, rs2, 21)
#define _FMAX_D(rd, rs1, rs2)          Rtype(83, rd, 1, rs1, rs2, 21)
#define _FCVT_S_D(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 1, 32)
#define _FCVT_D_S(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 0, 33)
#define _FEQ_D(rd, rs1, rs2)           Rtype(83, rd, 2, rs1, rs2, 81)
#define _FLT_D(rd, rs1, rs2)           Rtype(83, rd, 1, rs1, rs2, 81)
#define _FLE_D(rd, rs1, rs2)           Rtype(83, rd, 0, rs1, rs2, 81)
#define _FCLASS_D(rd, rs1)             Rtype(83, rd, 1, rs1, 0, 113)
#define _FCVT_W_D(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 0, 97)
#define _FCVT_WU_D(rd, rs1, rm)        Rtype(83, rd, rm, rs1, 1, 97)
#define _FCVT_D_W(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 0, 105)
#define _FCVT_D_WU(rd, rs1, rm)        Rtype(83, rd, rm, rs1, 1, 105)
/*
 * RV64D Standard Extension (in addition to RV32D)
 */
#define _FCVT_L_D(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 2, 97)
#define _FCVT_LU_D(rd, rs1, rm)        Rtype(83, rd, rm, rs1, 3, 97)
#define _FMV_X_D(rd, rs1)              Rtype(83, rd, 0, rs1, 0, 113)
#define _FCVT_D_L(rd, rs1, rm)         Rtype(83, rd, rm, rs1, 2, 105)
#define _FCVT_D_LU(rd, rs1, rm)        Rtype(83, rd, rm, rs1, 3, 105)
#define _FMV_D_X(rd, rs1)              Rtype(83, rd, 0, rs1, 0, 121)
/*
 * Pseudo instructions
 */
#define _FMV_S(r0, r1)                 _FSGNJ_S(r0, r1, r1)
#define _FABS_S(r0, r1)                _FSGNJX_S(r0, r1, r1)
#define _FNEG_S(r0, r1)                _FSGNJN_S(r0, r1, r1)
#define _FMV_D(r0, r1)                 _FSGNJ_D(r0, r1, r1)
#define _FABS_D(r0, r1)                _FSGNJX_D(r0, r1, r1)
#define _FNEG_D(r0, r1)                _FSGNJN_D(r0, r1, r1)

// Binary ALU operations
static void addr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void addr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void subr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void subr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void mulr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void mulr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void divr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void divr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);

// Unary ALU operations
static void sqrtr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static void sqrtr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static void negr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static void negr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static void absr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static void absr_d(jit_state_t *_jit, int32_t r0, int32_t r1);

// Transfer operations
static void movr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static void movr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static void movr_i_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static void movr_l_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static void movr_f_i(jit_state_t *_jit, int32_t r0, int32_t r1);
static void movr_d_l(jit_state_t *_jit, int32_t r0, int32_t r1);

// Argument management
static void retr_f(jit_state_t *_jit, int32_t u);
static void retr_d(jit_state_t *_jit, int32_t u);

// Load operations
static void ldr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static void ldr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static void ldi_f(jit_state_t *_jit, int32_t r0, jit_word_t i0);
static void ldxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void ldxi_f(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void ldi_d(jit_state_t *_jit, int32_t r0, jit_word_t i0);
static void ldxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void ldxi_d(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);

// Store operations
static void str_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static void str_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static void sti_f(jit_state_t *_jit, jit_word_t i0, int32_t r0);
static void stxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void stxi_f(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1);
static void sti_d(jit_state_t *_jit, jit_word_t i0, int32_t r0);
static void stxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void stxi_d(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1);

// Branch instructions
static jit_reloc_t bltr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bler_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t beqr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bger_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bner_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bunltr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bunler_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t buneqr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bunger_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bungtr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bltgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bordr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bunordr_f(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bltr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bler_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t beqr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bger_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bner_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bunltr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bunler_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t buneqr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bunger_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bungtr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bltgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bordr_d(jit_state_t *_jit, int32_t r0, int32_t r1);

/*
 * Binary ALU operations
 */
static void
addr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _FADD_S(r0, r1, r2));
}
static void
addr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _FADD_D(r0, r1, r2));
}
static void
subr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _FSUB_S(r0, r1, r2));
}
static void
subr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _FSUB_D(r0, r1, r2));
}
static void
mulr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _FMUL_S(r0, r1, r2));
}
static void
mulr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _FMUL_D(r0, r1, r2));
}
static void
divr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _FDIV_S(r0, r1, r2));
}
static void
divr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _FDIV_D(r0, r1, r2));
}

/*
 * Unary ALU operations
 */
static void
sqrtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FSQRT_S(r0, r1));
}
static void
sqrtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FSQRT_D(r0, r1));
}
static void
negr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FNEG_S(r0, r1));
}
static void
negr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FNEG_D(r0, r1));
}
static void
absr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FABS_S(r0, r1));
}

static void
absr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FABS_D(r0, r1));
}


/*
 * Load operations
 */
static void
ldr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FLW(r0, r1, 0));
}
static void
ldr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FLD(r0, r1, 0));
}
static void
ldi_f(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  ldr_f(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
ldxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r1, r2);
  ldr_f(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
ldxi_f(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0))
    em_wp(_jit, _FLW(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    ldr_f(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}
static void
ldi_d(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  ldr_d(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
ldxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r1, r2);
  ldr_d(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
ldxi_d(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0))
    em_wp(_jit, _FLD(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    ldr_d(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}



/*
 * Store operations
 */
static void
str_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FSW(r0, r1, 0));
}
static void
str_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FSD(r0, r1, 0));
}
static void
sti_f(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  str_f(_jit, jit_gpr_regno(t0), r0);
  unget_temp_gpr(_jit);
}
static void
stxr_f(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r0, r1);
  str_f(_jit, jit_gpr_regno(t0), r2);
  unget_temp_gpr(_jit);
}
static void
stxi_f(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (simm12_p(i0))
    em_wp(_jit, _FSW(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r0, i0);
    str_f(_jit, jit_gpr_regno(t0), r1);
    unget_temp_gpr(_jit);
  }
}
static void
sti_d(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  str_d(_jit, jit_gpr_regno(t0), r0);
  unget_temp_gpr(_jit);
}
static void
stxr_d(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r0, r1);
  str_d(_jit, jit_gpr_regno(t0), r2);
  unget_temp_gpr(_jit);
}
static void
stxi_d(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (simm12_p(i0))
    em_wp(_jit, _FSD(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r0, i0);
    str_d(_jit, jit_gpr_regno(t0), r1);
    unget_temp_gpr(_jit);
  }
}


/*
 * Transfer operations
 */
static void
movr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    em_wp(_jit, _FMV_S(r0, r1));
}

static void
movr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    em_wp(_jit, _FMV_D(r0, r1));
}
static void
movr_i_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FMV_X_W(r0, r1));
}
static void
movr_f_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FMV_W_X(r0, r1));
}
static void
movr_l_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FMV_X_D(r0, r1));
}
static void
movr_d_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FMV_D_X(r0, r1));
}

static void
truncr_f_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FCVT_W_S(r0, r1, 1));
}
static void
truncr_d_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FCVT_W_D(r0, r1, 1));
}
static void
truncr_f_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FCVT_L_S(r0, r1, 1));
}
static void
truncr_d_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FCVT_L_D(r0, r1, 1));
}

static void
extr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
#if __WORDSIZE == 64
  em_wp(_jit, _FCVT_S_L(r0, r1, 0));
#elif __WORDSIZE == 32
  em_wp(_jit, _FCVT_S_W(r0, r1, 0));
#endif
}
static void
extr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
#if __WORDSIZE == 64
  em_wp(_jit, _FCVT_D_L(r0, r1, 0));
#elif __WORDSIZE == 32
  em_wp(_jit, _FCVT_D_W(r0, r1, 0));
#endif
}

static void
extr_f_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FCVT_D_S(r0, r1, 0));
}
static void
extr_d_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _FCVT_S_D(r0, r1, 0));
}

static void
movi_f(jit_state_t *_jit, int32_t r0, jit_float32_t i0)
{
  union { int32_t i; jit_float32_t f; } u = { .f = i0 };
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), u.i);
  em_wp(_jit, _FMV_W_X(r0, jit_gpr_regno(reg)));
  unget_temp_gpr(_jit);
}
static void
movi_d(jit_state_t *_jit, int32_t r0, jit_float64_t i0)
{
  // TODO: How to move a 64 bit value from a 32 bit X register?
  // ATM only works on RV64
  union { int64_t i; jit_float64_t f; } u = { .f = i0 };
  jit_gpr_t reg = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(reg), u.i);
  em_wp(_jit, _FMV_D_X(r0, jit_gpr_regno(reg)));
  unget_temp_gpr(_jit);
}


/*
 * Argument management
 */
static void
retval_f(jit_state_t *_jit, int32_t r0)
{
  movr_f(_jit, jit_fpr_regno(_FA0), r0);
}

static void
retval_d(jit_state_t *_jit, int32_t r0)
{
  movr_d(_jit, jit_fpr_regno(_FA0), r0);
}

static void
retr_f(jit_state_t *_jit, int32_t u)
{
  movr_f(_jit, jit_fpr_regno(_FA0), u);
  ret(_jit);
}

static void
retr_d(jit_state_t *_jit, int32_t u)
{
  movr_d(_jit, jit_fpr_regno(_FA0), u);
  ret(_jit);
}


/*
 * Branch instructions
 */

static jit_reloc_t
bltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLT_S(t0, r0, r1));
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLE_S(t0, r0, r1));
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
beqr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FEQ_S(t0, r0, r1));
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bger_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bler_f(_jit, r1, r0);
}

static jit_reloc_t
bgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bltr_f(_jit, r1, r0);
}

static jit_reloc_t
bner_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FEQ_S(t0, r0, r1));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bunltr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLE_S(t0, r1, r0));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bunler_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLT_S(t0, r1, r0));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
buneqr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t t0 = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t t1 = jit_gpr_regno(get_temp_gpr(_jit));

  em_wp(_jit, _FLT_S(t0, r0, r1));
  em_wp(_jit, _FLT_S(t1, r1, r0));
  orr(_jit, t0, t0, t1);
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bunger_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLT_S(t0, r0, r1));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bungtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLE_S(t0, r0, r1));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bltgtr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t t0 = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t t1 = jit_gpr_regno(get_temp_gpr(_jit));

  em_wp(_jit, _FLT_S(t0, r1, r0));
  em_wp(_jit, _FLT_S(t1, r0, r1));
  orr(_jit, t0, t0, t1);
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t t0 = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t t1 = jit_gpr_regno(get_temp_gpr(_jit));

  em_wp(_jit, _FEQ_S(t0, r0, r0));
  em_wp(_jit, _FEQ_S(t1, r1, r1));
  andr(_jit, t0, t0, t1);
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bunordr_f(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t t0 = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t t1 = jit_gpr_regno(get_temp_gpr(_jit));

  em_wp(_jit, _FEQ_S(t0, r1, r1));
  em_wp(_jit, _FEQ_S(t1, r0, r0));
  andr(_jit, t0, t0, t1);
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLT_D(t0, r0, r1));
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLE_D(t0, r0, r1));
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
beqr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FEQ_D(t0, r0, r1));
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bger_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bler_d(_jit, r1, r0);
}

static jit_reloc_t
bgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bltr_d(_jit, r1, r0);
}

static jit_reloc_t
bner_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FEQ_D(t0, r0, r1));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bunltr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLE_D(t0, r1, r0));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bunler_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLT_D(t0, r1, r0));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
buneqr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t t0 = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t t1 = jit_gpr_regno(get_temp_gpr(_jit));

  em_wp(_jit, _FLT_D(t0, r0, r1));
  em_wp(_jit, _FLT_D(t1, r1, r0));
  orr(_jit, t0, t0, t1);
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bunger_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLT_D(t0, r0, r1));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bungtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  em_wp(_jit, _FLE_D(t0, r0, r1));
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bltgtr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t t0 = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t t1 = jit_gpr_regno(get_temp_gpr(_jit));

  em_wp(_jit, _FLT_D(t0, r1, r0));
  em_wp(_jit, _FLT_D(t1, r0, r1));
  orr(_jit, t0, t0, t1);
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t t0 = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t t1 = jit_gpr_regno(get_temp_gpr(_jit));

  em_wp(_jit, _FEQ_D(t0, r0, r0));
  em_wp(_jit, _FEQ_D(t1, r1, r1));
  andr(_jit, t0, t0, t1);
  jit_reloc_t ret = bner(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bunordr_d(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int32_t t0 = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t t1 = jit_gpr_regno(get_temp_gpr(_jit));

  em_wp(_jit, _FEQ_D(t0, r1, r1));
  em_wp(_jit, _FEQ_D(t1, r0, r0));
  andr(_jit, t0, t0, t1);
  jit_reloc_t ret = beqr(_jit, t0, jit_gpr_regno(_ZERO));

  unget_temp_gpr(_jit);
  return ret;
}
