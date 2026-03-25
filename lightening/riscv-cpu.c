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

#if __BYTE_ORDER != __LITTLE_ENDIAN
#error RISC-V requires little-endian host
#endif

#define stack_framesize   (200 + 64)
#define simm6_p(im)       ((im) <= 31 && (im) >= -32)
#define simm12_p(im)      ((im) <= 2047 && (im) >= -2048)
#define simm20_p(im)      ((im) <= 524287 && (im) >= -524288)
#define simm32_p(im)      ((im) <= 2147483647LL && (im) >= -2147483648LL)

typedef union {
  struct  {
    uint32_t opcode     : 7;
    uint32_t rd         : 5;
    uint32_t funct3     : 3;
    uint32_t rs1        : 5;
    uint32_t rs2        : 5;
    uint32_t funct7     : 7;
  } R;
  struct  {
    uint32_t opcode     : 7;
    uint32_t rd         : 5;
    uint32_t funct3     : 3;
    uint32_t rs1        : 5;
    uint32_t rs2        : 5;
    uint32_t rl         : 1;
    uint32_t aq         : 1;
    uint32_t funct5     : 5;
  } R4;
  struct  {
    uint32_t opcode     : 7;
    uint32_t rd         : 5;
    uint32_t funct3     : 3;
    uint32_t rs1        : 5;
    uint32_t imm11_0    : 12;
  } I;
#if __WORDSIZE == 64
  struct  {
    uint32_t opcode     : 7;
    uint32_t rd         : 5;
    uint32_t funct3     : 3;
    uint32_t rs1        : 5;
    uint32_t shamt      : 6;
    uint32_t imm6_0     : 6;
  } IS;
#endif
  struct  {
    uint32_t opcode     : 7;
    uint32_t imm4_0     : 5;
    uint32_t funct3     : 3;
    uint32_t rs1        : 5;
    uint32_t rs2        : 5;
    uint32_t imm11_5    : 7;
  } S;
  struct  {
    uint32_t opcode     : 7;
    uint32_t imm11      : 1;
    uint32_t imm4_1     : 4;
    uint32_t funct3     : 3;
    uint32_t rs1        : 5;
    uint32_t rs2        : 5;
    uint32_t imm10_5    : 6;
    uint32_t imm12      : 1;
  } B;
  struct  {
    uint32_t opcode     : 7;
    uint32_t rd         : 5;
    uint32_t imm31_12   : 20;
  } U;
  struct  {
    uint32_t opcode     : 7;
    uint32_t rd         : 5;
    uint32_t imm19_12   : 8;
    uint32_t imm11      : 1;
    uint32_t imm10_1    : 10;
    uint32_t imm20      : 1;
  } J;
  uint32_t w;
} instr_t;


// TODO: Compressed instruction support

static uint32_t
Rtype(int32_t op, int32_t rd, int32_t fct, int32_t rs1, int32_t rs2,
      int32_t fct2)
{
  instr_t     i;
  assert(!(op   & ~0x7f));
  assert(!(rd   & ~0x1f));
  assert(!(fct  & ~0x07));
  assert(!(rs1  & ~0x1f));
  assert(!(rs2  & ~0x1f));
  assert(!(fct2 & ~0x7f));
  i.R.opcode  = op;
  i.R.rd      = rd;
  i.R.funct3  = fct;
  i.R.rs1     = rs1;
  i.R.rs2     = rs2;
  i.R.funct7  = fct2;
  return i.w;
}

static uint32_t
R4type(int32_t op, int32_t rd, int32_t fct, int32_t rs1, int32_t rs2,
       int32_t aq, int32_t rl, int32_t fct5)
{
  instr_t     i;
  assert(!(op   & ~0x7f));
  assert(!(rd   & ~0x1f));
  assert(!(fct  & ~0x07));
  assert(!(rs1  & ~0x1f));
  assert(!(rs2  & ~0x1f));
  assert(!(fct5 & ~0x1f));
  assert(!(aq   & ~0x01));
  assert(!(rl   & ~0x01));
  i.R4.opcode = op;
  i.R4.rd     = rd;
  i.R4.funct3 = fct;
  i.R4.rs1    = rs1;
  i.R4.rs2    = rs2;
  i.R4.aq     = aq;
  i.R4.rl     = rl;
  i.R4.funct5 = fct5;
  return i.w;
}

static uint32_t
Itype(int32_t op, int32_t rd, int32_t fct, int32_t rs1, int32_t imm)
{
  instr_t     i;
  assert(!(op  &  ~0x7f));
  assert(!(rd  &  ~0x1f));
  assert(!(fct &  ~0x07));
  assert(!(rs1 &  ~0x1f));
  assert(simm12_p(imm));
  i.I.opcode  = op;
  i.I.rd      = rd;
  i.I.funct3  = fct;
  i.I.rs1     = rs1;
  i.I.imm11_0 = imm;
  return i.w;
}

#  if __WORDSIZE == 64
  static uint32_t
IStype(int32_t op, int32_t rd, int32_t fct, int32_t rs1, int32_t sh,
       int32_t imm)
{
  instr_t     i;
  assert(!(op  &  ~0x7f));
  assert(!(rd  &  ~0x1f));
  assert(!(fct &  ~0x07));
  assert(!(rs1 &  ~0x1f));
  assert(!(sh  &  ~0x3f));
  assert(simm6_p(imm));
  i.IS.opcode = op;
  i.IS.rd     = rd;
  i.IS.funct3 = fct;
  i.IS.rs1    = rs1;
  i.IS.shamt  = sh;
  i.IS.imm6_0 = imm;
  return i.w;
}
#  endif

static uint32_t
Stype(int32_t op, int32_t fct, int32_t rs1, int32_t rs2, int32_t imm)
{
  instr_t     i;
  assert(!(op  &  ~0x7f));
  assert(!(fct &  ~0x07));
  assert(!(rs1 &  ~0x1f));
  assert(!(rs2 &  ~0x1f));
  assert(simm12_p(imm));
  i.S.opcode  = op;
  i.S.imm4_0  = imm & 0x1f;
  i.S.funct3  = fct;
  i.S.rs1     = rs1;
  i.S.rs2     = rs2;
  i.S.imm11_5 = (imm >> 5) & 0x7f;
  return i.w;
}

static uint32_t
Btype(int32_t op, int32_t fct, int32_t rs1, int32_t rs2, int32_t imm)
{
  instr_t     i;
  assert(!(op  & ~0x7f));
  assert(!(fct & ~0x07));
  assert(!(rs1 & ~0x1f));
  assert(!(rs2 & ~0x1f));
  assert(!(imm & 1));
  assert(simm12_p(imm >> 1));

  i.B.opcode  = op;
  i.B.imm11   = (imm >> 11) & 0x1;
  i.B.imm4_1  = (imm >>  1) & 0xf;
  i.B.funct3  = fct;
  i.B.rs1     = rs1;
  i.B.rs2     = rs2;
  i.B.imm10_5 = (imm >>  5) & 0x3f;
  i.B.imm12   = (imm >> 12) & 0x1;
  return i.w;
}

static uint32_t
Utype(int32_t op, int32_t rd, int32_t imm)
{
  instr_t     i;
  assert(!(op & ~0x7f));
  assert(!(rd & ~0x1f));
  assert(simm20_p(imm));
  i.U.opcode  = op;
  i.U.rd      = rd;
  i.U.imm31_12= imm;
  return i.w;
}

static uint32_t
Jtype(int32_t op, int32_t rd, int32_t imm)
{
  instr_t     i;
  assert(!(op & ~0x7f));
  assert(!(rd & ~0x1f));
  assert(!(imm & 1));
  assert(simm20_p(imm >> 1));

  i.J.opcode  = op;
  i.J.rd      = rd;
  i.J.imm19_12= (imm >> 12) &  0xff;
  i.J.imm11   = (imm >> 11) &   0x1;
  i.J.imm10_1 = (imm >>  1) & 0x3ff;
  i.J.imm20   = (imm >> 20) &   0x1;
  return i.w;
}

/*
 * RV32I Base Instruction Set
 */
#define _LUI(rd, imm)              Utype(55, rd, imm)
#define _AUIPC(rd, imm)            Utype(23, rd, imm)
#define _JAL(rd, imm)              Jtype(111, rd, imm)
#define _JALR(rd, rs1, imm)        Itype(103, rd, 0, rs1, imm)
#define _BEQ(rs1, rs2, imm)        Btype(99, 0, rs1, rs2, imm)
#define _BNE(rs1, rs2, imm)        Btype(99, 1, rs1, rs2, imm)
#define _BLT(rs1, rs2, imm)        Btype(99, 4, rs1, rs2, imm)
#define _BGE(rs1, rs2, imm)        Btype(99, 5, rs1, rs2, imm)
#define _BLTU(rs1, rs2, imm)       Btype(99, 6, rs1, rs2, imm)
#define _BGEU(rs1, rs2, imm)       Btype(99, 7, rs1, rs2, imm)
#define _LB(rd, rs1, imm)          Itype(3, rd, 0, rs1, imm)
#define _LH(rd, rs1, imm)          Itype(3, rd, 1, rs1, imm)
#define _LW(rd, rs1, imm)          Itype(3, rd, 2, rs1, imm)
#define _LBU(rd, rs1, imm)         Itype(3, rd, 4, rs1, imm)
#define _LHU(rd, rs1, imm)         Itype(3, rd, 5, rs1, imm)
#define _SB(rs1, rs2, imm)         Stype(35, 0, rs1, rs2, imm)
#define _SH(rs1, rs2, imm)         Stype(35, 1, rs1, rs2, imm)
#define _SW(rs1, rs2, imm)         Stype(35, 2, rs1, rs2, imm)
#define _ADDI(rd, rs1, imm)        Itype(19, rd, 0, rs1, imm)
#define _SLTI(rd, rs1, imm)        Itype(19, rd, 2, rs1, imm)
#define _SLTIU(rd, rs1, imm)       Itype(19, rd, 3, rs1, imm)
#define _XORI(rd, rs1, imm)        Itype(19, rd, 4, rs1, imm)
#define _ORI(rd, rs1, imm)         Itype(19, rd, 6, rs1, imm)
#define _ANDI(rd, rs1, imm)        Itype(19, rd, 7, rs1, imm)
#if __WORDSIZE == 32
#  define _SLLI(rd, rs1, imm)      Rtype(19, rd, 1, rs1, imm,  0)
#  define _SRLI(rd, rs1, imm)      Rtype(19, rd, 5, rs1, imm,  0)
#  define _SRAI(rd, rs1, imm)      Rtype(19, rd, 5, rs1, imm, 32)
#endif
#define _ADD(rd, rs1, rs2)         Rtype(51, rd, 0, rs1, rs2,  0)
#define _SUB(rd, rs1, rs2)         Rtype(51, rd, 0, rs1, rs2, 32)
#define _SLL(rd, rs1, rs2)         Rtype(51, rd, 1, rs1, rs2,  0)
#define _SLT(rd, rs1, rs2)         Rtype(51, rd, 2, rs1, rs2,  0)
#define _SLTU(rd, rs1, rs2)        Rtype(51, rd, 3, rs1, rs2,  0)
#define _XOR(rd, rs1, rs2)         Rtype(51, rd, 4, rs1, rs2,  0)
#define _SRL(rd, rs1, rs2)         Rtype(51, rd, 5, rs1, rs2,  0)
#define _SRA(rd, rs1, rs2)         Rtype(51, rd, 5, rs1, rs2, 32)
#define _OR(rd, rs1, rs2)          Rtype(51, rd, 6, rs1, rs2,  0)
#define _AND(rd, rs1, rs2)         Rtype(51, rd, 7, rs1, rs2,  0)
#define _FENCE(imm)                Itype( 15,  0, 0,    0, imm)
#define _FENCE_I(imm)              Itype( 15,  0, 1,    0, imm)
#define _ECALL()                   Itype(115,  0, 0,    0,   0)
#define _EBREAK()                  Itype(115,  0, 0,    0,   1)
#define _CSRRW(rd, rs1, csr)       Itype(115, rd, 1,  rs1, csr)
#define _CSRRS(rd, rs1, csr)       Itype(115, rd, 2,  rs1, csr)
#define _CSRRC(rd, rs1, csr)       Itype(115, rd, 3,  rs1, csr)
#define _CSRRWI(rd, zimm, csr)     Itype(115, rd, 5, zimm, csr)
#define _CSRRSI(rd, zimm, csr)     Itype(115, rd, 6, zimm, csr)
#define _CSRRCI(rd, zimm, csr)     Itype(115, rd, 7, zimm, csr)
/*
 * RV64I Base Instruction Set (in addition to RV32I)
 */
#define _LWU(rd, rs1, imm)         Itype(3, rd, 6, rs1, imm)
#define _LD(rd, rs1, imm)          Itype(3, rd, 3, rs1, imm)
#define _SD(rs1, rs2, imm)         Stype(35, 3, rs1, rs2, imm)
#if __WORDSIZE == 64
#  define _SLLI(rd, rs1, sh)       IStype(19, rd, 1, rs1, sh,  0)
#  define _SRLI(rd, rs1, sh)       IStype(19, rd, 5, rs1, sh,  0)
#  define _SRAI(rd, rs1, sh)       IStype(19, rd, 5, rs1, sh, 16)
#endif
#define _ADDIW(rd, rs1, imm)       Itype(27, rd, 0, rs1, imm)
#define _SLLIW(rd, rs1, imm)       Rtype(27, rd, 1, rs1, imm,  0)
#define _SRLIW(rd, rs1, imm)       Rtype(27, rd, 3, rs1, imm,  0)
#define _SRAIW(rd, rs1, imm)       Rtype(27, rd, 3, rs1, imm, 32)
#define _ADDW(rd, rs1, imm)        Rtype(59, rd, 0, rs1, imm,  0)
#define _SUBW(rd, rs1, imm)        Rtype(59, rd, 0, rs1, imm, 32)
#define _SLLW(rd, rs1, imm)        Rtype(59, rd, 1, rs1, imm,  0)
#define _SRLW(rd, rs1, imm)        Rtype(59, rd, 5, rs1, imm,  0)
#define _SRAW(rd, rs1, imm)        Rtype(59, rd, 5, rs1, imm, 32)
/*
 * RV32M Standard Extension
 */
#define _MUL(rd, rs1, rs2)         Rtype(51, rd, 0, rs1, rs2, 1)
#define _MULH(rd, rs1, rs2)        Rtype(51, rd, 1, rs1, rs2, 1)
#define _MULHSU(rd, rs1, rs2)      Rtype(51, rd, 2, rs1, rs2, 1)
#define _MULHU(rd, rs1, rs2)       Rtype(51, rd, 3, rs1, rs2, 1)
#define _DIV(rd, rs1, rs2)         Rtype(51, rd, 4, rs1, rs2, 1)
#define _DIVU(rd, rs1, rs2)        Rtype(51, rd, 5, rs1, rs2, 1)
#define _REM(rd, rs1, rs2)         Rtype(51, rd, 6, rs1, rs2, 1)
#define _REMU(rd, rs1, rs2)        Rtype(51, rd, 7, rs1, rs2, 1)
/*
 * RV64M Standard Extension (in addition to RV32M)
 */
#define _MULW(rd, rs1, rs2)        Rtype(59, rd, 0, rs1, rs2, 1)
#define _DIVW(rd, rs1, rs2)        Rtype(59, rd, 4, rs1, rs2, 1)
#define _DIVUW(rd, rs1, rs2)       Rtype(59, rd, 5, rs1, rs2, 1)
#define _REMW(rd, rs1, rs2)        Rtype(59, rd, 6, rs1, rs2, 1)
#define _REMUW(rd, rs1, rs2)       Rtype(59, rd, 7, rs1, rs2, 1)
/*
 * RV32A Standard Extension
 */
#define _LR_W(rd, rs1, rl, aq)             R4type(47, rd, 2, rs1,   0, rl, aq,  2)
#define _SC_W(rd, rs1, rs2, rl, aq)        R4type(47, rd, 2, rs1, rs2, rl, aq,  3)
#define _AMOSWAP_W(rd, rs1, rs2, rl, aq)   R4type(47, rd, 2, rs1, rs2, rl, aq,  1)
#define _AMOADD_W(rd, rs1, rs2, rl, aq)    R4type(47, rd, 2, rs1, rs2, rl, aq,  0)
#define _AMOXOR_W(rd, rs1, rs2, rl, aq)    R4type(47, rd, 2, rs1, rs2, rl, aq,  4)
#define _AMOAND_W(rd, rs1, rs2, rl, aq)    R4type(47, rd, 2, rs1, rs2, rl, aq, 12)
#define _AMOOR_W(rd, rs1, rs2, rl, aq)     R4type(47, rd, 2, rs1, rs2, rl, aq,  8)
#define _AMOMIN_W(rd, rs1, rs2, rl, aq)    R4type(47, rd, 2, rs1, rs2, rl, aq, 16)
#define _AMOMAX_W(rd, rs1, rs2, rl, aq)    R4type(47, rd, 2, rs1, rs2, rl, aq, 20)
#define _AMOMINU_W(rd, rs1, rs2, rl, aq)   R4type(47, rd, 2, rs1, rs2, rl, aq, 24)
#define _AMOMAXU_W(rd, rs1, rs2, rl, aq)   R4type(47, rd, 2, rs1, rs2, rl, aq, 28)
/*
 * RV64A Standard Extension (in addition to RV32A)
 */
#define _LR_D(rd, rs1, rl, aq)             R4type(47, rd, 3, rs1,   0, rl, aq,  2)
#define _SC_D(rd, rs1, rs2, rl, aq)        R4type(47, rd, 3, rs1, rs2, rl, aq,  3)
#define _AMOSWAP_D(rd, rs1, rs2, rl, aq)   R4type(47, rd, 3, rs1, rs2, rl, aq,  1)
#define _AMOADD_D(rd, rs1, rs2, rl, aq)    R4type(47, rd, 3, rs1, rs2, rl, aq,  0)
#define _AMOXOR_D(rd, rs1, rs2, rl, aq)    R4type(47, rd, 3, rs1, rs2, rl, aq,  4)
#define _AMOAND_D(rd, rs1, rs2, rl, aq)    R4type(47, rd, 3, rs1, rs2, rl, aq, 12)
#define _AMOOR_D(rd, rs1, rs2, rl, aq)     R4type(47, rd, 3, rs1, rs2, rl, aq,  8)
#define _AMOMIN_D(rd, rs1, rs2, rl, aq)    R4type(47, rd, 3, rs1, rs2, rl, aq, 16)
#define _AMOMAX_D(rd, rs1, rs2, rl, aq)    R4type(47, rd, 3, rs1, rs2, rl, aq, 20)
#define _AMOMINU_D(rd, rs1, rs2, rl, aq)   R4type(47, rd, 3, rs1, rs2, rl, aq, 24)
#define _AMOMAXU_D(rd, rs1, rs2, rl, aq)   R4type(47, rd, 3, rs1, rs2, rl, aq, 28)
/*
 * Pseudo Instructions
 */
#define _NOP()                      _ADDI((jit_gpr_regno(_ZERO)),\
                                          (jit_gpr_regno(_ZERO)), 0)
#define _MV(r0, r1)                _ADDI(r0, r1, 0)
#define _NOT(r0, r1)               _XORI(r0, r1, -1)
#define _NEG(r0, r1)               _SUB(r0, (jit_gpr_regno(_ZERO)), r1)
#define _NEGW(r0, r1)              _SUBW(r0, (jit_gpr_regno(_ZERO)), r1)
#define _SEXT_W(r0, r1)            _ADDIW(r0, r1, 0)
#define _RET()                     _JALR((jit_gpr_regno(_ZERO)),\
                                         (jit_gpr_regno(_RA)), 0)



// Help to make all easier
#define em_wp(jit, inst)          emit_u32_with_pool(jit, inst)

/*
 * JIT INSTRUCTIONS
 */

// Binary ALU operations
static void addr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void addi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void addcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void addci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void addxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void addxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);

static void subr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void subi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void subcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void subci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void subxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void subxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);

static void muli(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void mulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);

static void divr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void divi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void divr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void divi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);

static void remi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void remr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void remi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void remr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);

static void andr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void andi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void orr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void ori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void xorr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void xori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void lshi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void lshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void rshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void rshi(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0);
static void rshr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void rshi_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0);


// Four operand ALU operations
static void qmulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3);
static void qmulr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3);
static void qmuli(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0);
static void qmuli_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0);

static void qdivr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3);
static void qdivr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3);
static void qdivi(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0);
static void qdivi_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0);


// Unary ALU operations
static void negr(jit_state_t *_jit, int32_t r0, int32_t r1);
static void comr(jit_state_t *_jit, int32_t r0, int32_t r1);


// Transfer operations
static void movr(jit_state_t *_jit, int32_t r0, int32_t r1);
static void movi(jit_state_t *_jit, int32_t r0, jit_word_t i0);

static uint64_t patch_load_from_pool(uint64_t instrs, int32_t off);
static jit_reloc_t emit_load_from_pool(jit_state_t *_jit, uint64_t insts);
static jit_reloc_t mov_addr(jit_state_t *_jit, int32_t r0);
static jit_reloc_t movi_from_pool(jit_state_t *_jit, int32_t r0);

static void extr_c(jit_state_t *_jit, int32_t r0, int32_t r1);
static void extr_uc(jit_state_t *_jit, int32_t r0, int32_t r1);
static void extr_s(jit_state_t *_jit, int32_t r0, int32_t r1);
static void extr_us(jit_state_t *_jit, int32_t r0, int32_t r1);

#  if __WORDSIZE == 64
static void extr_i(jit_state_t *_jit, int32_t r0, int32_t r1);
static void extr_ui(jit_state_t *_jit, int32_t r0, int32_t r1);
#endif


// Branch instructions
static uint32_t patch_cc_jump(uint32_t inst, int32_t offset);
static jit_reloc_t emit_cc_jump(jit_state_t *_jit, uint32_t inst);

static jit_reloc_t bltr(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t blti(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bltr_u(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t blti_u(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bler(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t blei(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bler_u(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t blei_u(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t beqr(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t beqi(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bger(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bgei(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bger_u(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bgei_u(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bgtr(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bgti(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bgtr_u(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bgti_u(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bner(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bnei(jit_state_t *_jit, int32_t r0, jit_word_t i1);

static jit_reloc_t bmsr(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bmsi(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bmcr(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bmci(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t boaddr(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t boaddi(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t boaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t boaddi_u(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bxaddr(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bxaddi(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bxaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bxaddi_u(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bosubr(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bosubi(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bosubr_u(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bosubi_u(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bxsubr(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bxsubi(jit_state_t *_jit, int32_t r0, jit_word_t i1);
static jit_reloc_t bxsubr_u(jit_state_t *_jit, int32_t r0, int32_t r1);
static jit_reloc_t bxsubi_u(jit_state_t *_jit, int32_t r0, jit_word_t i1);


// Store operations
static void str_c(jit_state_t *_jit, int32_t r0, int32_t r1);
static void str_uc(jit_state_t *_jit, int32_t r0, int32_t r1);
static void str_s(jit_state_t *_jit, int32_t r0, int32_t r1);
static void str_i(jit_state_t *_jit, int32_t r0, int32_t r1);
#if __WORDSIZE == 64
static void str_l(jit_state_t *_jit, int32_t r0, int32_t r1);
#endif

static void sti_c(jit_state_t *_jit, jit_word_t i0, int32_t r0);
static void sti_s(jit_state_t *_jit, jit_word_t i0, int32_t r0);
static void sti_i(jit_state_t *_jit, jit_word_t i0, int32_t r0);
#if __WORDSIZE == 64
static void sti_l(jit_state_t *_jit, jit_word_t i0, int32_t r0);
#endif

static void stxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void stxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void stxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
# if __WORDSIZE == 64
static void stxr_l(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
#endif

static void stxi_c(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1);
static void stxi_s(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1);
static void stxi_i(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1);
# if __WORDSIZE == 64
static void stxi_l(jit_state_t *_jit,jit_word_t i0,int32_t r0,int32_t r1);
# endif


// Load operations
static void ldr_c(jit_state_t *_jit, int32_t r0, int32_t r1);
static void ldr_uc(jit_state_t *_jit, int32_t r0, int32_t r1);
static void ldr_s(jit_state_t *_jit, int32_t r0, int32_t r1);
static void ldr_us(jit_state_t *_jit, int32_t r0, int32_t r1);
static void ldr_i(jit_state_t *_jit, int32_t r0, int32_t r1);
# if __WORDSIZE == 64
static void ldr_ui(jit_state_t *_jit, int32_t r0, int32_t r1);
static void ldr_l(jit_state_t *_jit, int32_t r0, int32_t r1);
# endif

static void ldi_c(jit_state_t *_jit, int32_t r0, jit_word_t i0);
static void ldi_uc(jit_state_t *_jit, int32_t r0, jit_word_t i0);
static void ldi_s(jit_state_t *_jit, int32_t r0, jit_word_t i0);
static void ldi_us(jit_state_t *_jit, int32_t r0, jit_word_t i0);
static void ldi_i(jit_state_t *_jit, int32_t r0, jit_word_t i0);
# if __WORDSIZE == 64
static void ldi_ui(jit_state_t *_jit, int32_t r0, jit_word_t i0);
static void ldi_l(jit_state_t *_jit, int32_t r0, jit_word_t i0);
# endif

static void ldxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void ldxr_uc(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void ldxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void ldxr_us(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void ldxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
#  if __WORDSIZE == 64
static void ldxr_ui(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
static void ldxr_l(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2);
#endif

static void ldxi_c(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void ldxi_uc(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void ldxi_us(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void ldxi_s(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void ldxi_i(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
#  if __WORDSIZE == 64
static void ldxi_ui(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0);
static void ldxi_l(jit_state_t *_jit,int32_t r0,int32_t r1,jit_word_t i0);
#endif


// Argument management
//static void pushr(jit_state_t *_jit, int32_t r0);
//static void popr(jit_state_t *_jit, int32_t r0);
static void ret(jit_state_t *_jit);
static void retr(jit_state_t *_jit, int32_t r0);
static void reti(jit_state_t *_jit, jit_word_t i0);
static void retval_c(jit_state_t *_jit, int32_t r0);
static void retval_uc(jit_state_t *_jit, int32_t r0);
static void retval_s(jit_state_t *_jit, int32_t r0);
static void retval_us(jit_state_t *_jit, int32_t r0);
static void retval_i(jit_state_t *_jit, int32_t r0);
#  if __WORDSIZE == 64
static void retval_ui(jit_state_t *_jit, int32_t r0);
static void retval_l(jit_state_t *_jit, int32_t r0);
#endif

// Jump and return
static uint32_t patch_jump(uint32_t inst, int32_t offset);
static jit_reloc_t emit_jump(jit_state_t *_jit, uint32_t inst);

static void callr(jit_state_t *_jit, int32_t r0);
static void calli(jit_state_t *_jit, jit_word_t i0);
static void jmpi_with_link(jit_state_t *_jit, jit_word_t i0);
static void pop_link_register(jit_state_t *_jit);
static void push_link_register(jit_state_t *_jit);
static void jmpr(jit_state_t *_jit, int32_t r0);
static void jmpi(jit_state_t *_jit, jit_word_t i0);
static jit_reloc_t jmp(jit_state_t *_jit);


// Atomic operations
static void ldr_atomic(jit_state_t *_jit, int32_t dst, int32_t loc);
static void str_atomic(jit_state_t *_jit, int32_t loc, int32_t val);
static void swap_atomic(jit_state_t *_jit, int32_t dst, int32_t loc,
    int32_t val);
static void cas_atomic(jit_state_t *_jit, int32_t dst, int32_t loc,
    int32_t expected, int32_t desired);

// Byte swapping operations
static void bswapr_us(jit_state_t *_jit, int32_t r0, int32_t r1);
static void bswapr_ui(jit_state_t *_jit, int32_t r0, int32_t r1);
#  if __WORDSIZE == 64
static void
bswapr_ul(jit_state_t *_jit, int32_t r0, int32_t r1);
#endif

// Others
static void nop(jit_state_t *_jit, int32_t im);
static void mfence(jit_state_t *_jit);
static void breakpoint(jit_state_t *_jit);



/*
 * Binary ALU operations
 */
static void
addr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _ADD(r0, r1, r2));
}
static void
addi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0)){
    em_wp(_jit, _ADDI(r0, r1, i0));
  } else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(t0), i0);
    addr(_jit, r0, r1, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}

static void
addcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  // TODO: Not sure if this is correct
  jit_gpr_t t0;
  if (r0 == r1) {
    t0 = get_temp_gpr(_jit);
    addr(_jit, jit_gpr_regno(t0), r1, r2);
    em_wp(_jit, _SLTU(jit_gpr_regno(JIT_CARRY), jit_gpr_regno(t0), r1));
    movr(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
  else {
    addr(_jit, r0, r1, r2);
    em_wp(_jit, _SLTU(jit_gpr_regno(JIT_CARRY), r0, r1));
  }
}

static void
addci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t t0;
  if (r0 == r1) {
    t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    em_wp(_jit, _SLTU(jit_gpr_regno(JIT_CARRY), jit_gpr_regno(t0), r1));
    movr(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
  else {
    addi(_jit, r0, r1, i0);
    em_wp(_jit, _SLTU(jit_gpr_regno(JIT_CARRY), r0, r1));
  }
}

static void
addxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0;
  t0 = get_temp_gpr(_jit);
  movr(_jit, jit_gpr_regno(t0), jit_gpr_regno(JIT_CARRY));
  addcr(_jit, r0, r1, r2);
  addcr(_jit, r0, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
addxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t t0;
  t0 = get_temp_gpr(_jit);
  movr(_jit, jit_gpr_regno(t0), jit_gpr_regno(JIT_CARRY));
  addci(_jit, r0, r1, i0);
  addcr(_jit, r0, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
subr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _SUB(r0, r1, r2));
}

static void
subi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  addi(_jit, r0, r1, -i0);
}

static void
subcr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{

  jit_gpr_t t0;
  if (r0 == r1) {
    t0 = get_temp_gpr(_jit);
    subr(_jit, jit_gpr_regno(t0), r1, r2);
    em_wp(_jit, _SLTU(jit_gpr_regno(JIT_CARRY), r1, jit_gpr_regno(t0)));
    movr(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
  else {
    addr(_jit, r0, r1, r2);
    em_wp(_jit, _SLTU(jit_gpr_regno(JIT_CARRY), r1, r0));
  }
}

static void
subci(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{

  jit_gpr_t t0;
  if (r0 == r1) {
    t0 = get_temp_gpr(_jit);
    subi(_jit, jit_gpr_regno(t0), r1, i0);
    em_wp(_jit, _SLTU(jit_gpr_regno(JIT_CARRY), r1, jit_gpr_regno(t0)));
    movr(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
  else {
    addi(_jit, r0, r1, i0);
    em_wp(_jit, _SLTU(jit_gpr_regno(JIT_CARRY), r1, r0));
  }
}

static void
subxr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0;
  t0 = get_temp_gpr(_jit);
  movr(_jit, jit_gpr_regno(t0), jit_gpr_regno(JIT_CARRY));
  subcr(_jit, r0, r1, r2);
  subcr(_jit, r0, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
subxi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t t0;
  t0 = get_temp_gpr(_jit);
  movr(_jit, jit_gpr_regno(t0), jit_gpr_regno(JIT_CARRY));
  subci(_jit, r0, r1, i0);
  subcr(_jit, r0, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
muli(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  mulr(_jit, r0, r1,  jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
mulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _MUL(r0, r1, r2));
}

static void
divr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _DIV(r0, r1, r2));
}

static void
divi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  divr(_jit, r0, r1,  jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
divr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _DIVU(r0, r1, r2));
}

static void
divi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  divr_u(_jit, r0, r1,  jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
remi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  remr(_jit, r0, r1,  jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
remr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _REM(r0, r1, r2));
}
static void
remi_u(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  remr_u(_jit, r0, r1,  jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
remr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _REMU(r0, r1, r2));
}

static void
andr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _AND(r0, r1, r2));
}

static void
andi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0)){
    em_wp(_jit, _ANDI(r0, r1, i0));
  } else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(t0), i0);
    em_wp(_jit, _AND(r0, r1, jit_gpr_regno(t0)));
    unget_temp_gpr(_jit);
  }
}

static void
orr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _OR(r0, r1, r2));
}

static void
ori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0)){
    em_wp(_jit, _ORI(r0, r1, i0));
  } else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(t0), i0);
    orr(_jit, r0, r1, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}

static void
xorr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _XOR(r0, r1, r2));
}

static void
xori(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0)){
    em_wp(_jit, _XORI(r0, r1, i0));
  } else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(t0), i0);
    xorr(_jit, r0, r1, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}

static void
lshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _SLL(r0, r1, r2));
}

static void
lshi(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0)){
    em_wp(_jit, _SLLI(r0, r1, i0));
  } else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(t0), i0);
    lshr(_jit, r0, r1, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}

static void
rshr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _SRA(r0, r1, r2));
}

static void
rshi(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  if (simm12_p(i0)){
    em_wp(_jit, _SRAI(r0, r1, i0));
  } else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(t0), i0);
    rshr(_jit, r0, r1, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}

static void
rshr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  em_wp(_jit, _SRL(r0, r1, r2));
}

static void
rshi_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t i0)
{
  if (simm12_p(i0)){
    em_wp(_jit, _SRLI(r0, r1, i0));
  } else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(t0), i0);
    rshr_u(_jit, r0, r1, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}


/*
 * Four operand ALU operations
 */
static void
iqmulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3,
    jit_bool_t sign){
  if(r0 == r2 || r0 == r3){
    jit_gpr_t t0 = get_temp_gpr(_jit);
    em_wp(_jit, _MUL(jit_gpr_regno(t0), r2, r3));
    if(sign)
      em_wp(_jit, _MULH(r1, r2, r3));
    else
      em_wp(_jit, _MULHU(r1, r2, r3));
    movr(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
  em_wp(_jit, _MUL(r0, r2, r3));
  if(sign)
    em_wp(_jit, _MULH(r1, r2, r3));
  else
    em_wp(_jit, _MULHU(r1, r2, r3));
}

static void
qmulr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  iqmulr(_jit, r0, r1, r2, r3, 1);
}

static void
qmulr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  iqmulr(_jit, r0, r1, r2, r3, 0);
}

static void
qmuli(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  iqmulr(_jit, r0, r1, r2, jit_gpr_regno(t0), 1);
  unget_temp_gpr(_jit);
}

static void
qmuli_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  iqmulr(_jit, r0, r1, r2, jit_gpr_regno(t0), 0);
  unget_temp_gpr(_jit);
}

static void
iqdivr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3,
    jit_bool_t sign){
  if(r0 == r2 || r0 == r3){
    jit_gpr_t t0 = get_temp_gpr(_jit);
    if(sign){
      em_wp(_jit, _DIV(jit_gpr_regno(t0), r2, r3));
      em_wp(_jit, _REM(r1, r2, r3));
    } else {
      em_wp(_jit, _DIVU(jit_gpr_regno(t0), r2, r3));
      em_wp(_jit, _REMU(r1, r2, r3));
    }
    movr(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
  if(sign){
    em_wp(_jit, _DIV(r0, r2, r3));
    em_wp(_jit, _REM(r1, r2, r3));
  } else {
    em_wp(_jit, _DIVU(r0, r2, r3));
    em_wp(_jit, _REMU(r1, r2, r3));
  }
}

static void
qdivr(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  iqdivr(_jit, r0, r1, r2, r3, 1);
}

static void
qdivr_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, int32_t r3)
{
  iqdivr(_jit, r0, r1, r2, r3, 0);
}

static void
qdivi(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  iqdivr(_jit, r0, r1, r2, jit_gpr_regno(t0), 1);
  unget_temp_gpr(_jit);
}

static void
qdivi_u(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  iqdivr(_jit, r0, r1, r2, jit_gpr_regno(t0), 0);
  unget_temp_gpr(_jit);
}


/*
 * Unary ALU operations
 */
static void
negr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _NEG(r0, r1));
}

static void
comr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _NOT(r0, r1));
}


/*
 * Branch instructions
 */
static uint32_t
patch_cc_jump(uint32_t inst, int32_t offset){
  instr_t i;
  i.w = inst;
  return Btype(i.B.opcode, i.B.funct3, i.B.rs1, i.B.rs2, offset);
}

static jit_reloc_t
emit_cc_jump(jit_state_t *_jit, uint32_t inst)
{
  while (1) {
    uint8_t *pc_base = _jit->pc.uc;   // Offset is from current PC
    int32_t off = (uint8_t*)jit_address(_jit) - pc_base;
    jit_reloc_t ret =
      jit_reloc (_jit, JIT_RELOC_JCC_WITH_VENEER, 0, _jit->pc.uc, pc_base, 0);
    uint8_t cc_jump_width = 12;
    if (add_pending_literal(_jit, ret, cc_jump_width - 1)) {
      em_wp(_jit, patch_cc_jump(inst, off));
      return ret;
    }
  }
}

static jit_reloc_t
bltr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return emit_cc_jump(_jit, _BLT(r0, r1, 0));
}

static jit_reloc_t
blti(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{

  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bltr(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bltr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return emit_cc_jump(_jit, _BLTU(r0, r1, 0));
}

static jit_reloc_t
blti_u(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bltr_u(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bler(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return emit_cc_jump(_jit, _BGE(r1, r0, 0));
}

static jit_reloc_t
blei(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bler(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bler_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return emit_cc_jump(_jit, _BGEU(r1, r0, 0));
}

static jit_reloc_t
blei_u(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bler_u(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
beqr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return emit_cc_jump(_jit, _BEQ(r0, r1, 0));
}

static jit_reloc_t
beqi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = beqr(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bger(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return emit_cc_jump(_jit, _BGE(r0, r1, 0));
}

static jit_reloc_t
bgei(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bger(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bger_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return emit_cc_jump(_jit, _BGEU(r0, r1, 0));
}

static jit_reloc_t
bgei_u(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bger_u(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bgtr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bltr(_jit, r1, r0);
}

static jit_reloc_t
bgti(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bgtr(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bgtr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return bltr_u(_jit, r1, r0);
}

static jit_reloc_t
bgti_u(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bgtr_u(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bner(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  return emit_cc_jump(_jit, _BNE(r0, r1, 0));
}

static jit_reloc_t
bnei(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bner(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bmsr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  andr(_jit, jit_gpr_regno(t0), r0, r1);
  jit_reloc_t ret = bner(_jit, jit_gpr_regno(t0), jit_gpr_regno(_ZERO));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bmsi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  andi(_jit, jit_gpr_regno(t0), r0, i0);
  jit_reloc_t ret = bner(_jit, jit_gpr_regno(t0), jit_gpr_regno(_ZERO));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bmcr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  andr(_jit, jit_gpr_regno(t0), r0, r1);
  jit_reloc_t ret = beqr(_jit, jit_gpr_regno(t0), jit_gpr_regno(_ZERO));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bmci(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  andi(_jit, jit_gpr_regno(t0), r0, i0);
  jit_reloc_t ret = beqr(_jit, jit_gpr_regno(t0), jit_gpr_regno(_ZERO));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
boaddr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  // NOTE: We need tons of temporaries because RISC-V doesn't provide any
  // easy way to solve this. We need to do it in software.
  jit_gpr_t t0 = get_temp_gpr(_jit);
  jit_gpr_t t1 = get_temp_gpr(_jit);
  jit_gpr_t t2 = get_temp_gpr(_jit);

  addr(_jit, jit_gpr_regno(t0), r0, r1);

  em_wp(_jit, _SLTI(jit_gpr_regno(t1), r1, 0));
  em_wp(_jit, _SLT(jit_gpr_regno(t2), jit_gpr_regno(t0), r0));
  movr(_jit, r0, jit_gpr_regno(t0));
  jit_reloc_t ret = bner(_jit, jit_gpr_regno(t1), jit_gpr_regno(t2));

  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
boaddi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = boaddr(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
boaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  jit_gpr_t t1 = get_temp_gpr(_jit);

  addr(_jit, jit_gpr_regno(t0), r0, r1);

  em_wp(_jit, _SLTU(jit_gpr_regno(t1), jit_gpr_regno(t0), r0));
  movr(_jit, r0, jit_gpr_regno(t0));

  jit_reloc_t ret = bnei(_jit, jit_gpr_regno(t1), 0);

  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
boaddi_u(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = boaddr_u(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bxaddr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  jit_gpr_t t1 = get_temp_gpr(_jit);
  jit_gpr_t t2 = get_temp_gpr(_jit);

  addr(_jit, jit_gpr_regno(t0), r0, r1);

  em_wp(_jit, _SLTI(jit_gpr_regno(t1), r1, 0));
  em_wp(_jit, _SLT(jit_gpr_regno(t2), jit_gpr_regno(t0), r0));
  movr(_jit, r0, jit_gpr_regno(t0));
  jit_reloc_t ret = beqr(_jit, jit_gpr_regno(t1), jit_gpr_regno(t2));

  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bxaddi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{

  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bxaddr(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bxaddr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  jit_gpr_t t1 = get_temp_gpr(_jit);

  addr(_jit, jit_gpr_regno(t0), r0, r1);

  em_wp(_jit, _SLTU(jit_gpr_regno(t1), jit_gpr_regno(t0), r0));
  movr(_jit, r0, jit_gpr_regno(t0));

  jit_reloc_t ret = beqi(_jit, jit_gpr_regno(t1), 0);

  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bxaddi_u(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bxaddr_u(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bosubr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  jit_gpr_t t1 = get_temp_gpr(_jit);
  jit_gpr_t t2 = get_temp_gpr(_jit);

  subr(_jit, jit_gpr_regno(t0), r0, r1);

  em_wp(_jit, _SLTI(jit_gpr_regno(t1), r1, 0));
  em_wp(_jit, _SLT(jit_gpr_regno(t2), r0, jit_gpr_regno(t0)));
  movr(_jit, r0, jit_gpr_regno(t0));
  jit_reloc_t ret = bner(_jit, jit_gpr_regno(t1), jit_gpr_regno(t2));

  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bosubi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bosubr(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bosubr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  jit_gpr_t t1 = get_temp_gpr(_jit);

  subr(_jit, jit_gpr_regno(t0), r0, r1);

  em_wp(_jit, _SLTU(jit_gpr_regno(t1), r0, jit_gpr_regno(t0)));
  movr(_jit, r0, jit_gpr_regno(t0));
  jit_reloc_t ret = beqi(_jit, jit_gpr_regno(t1), 1);

  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bosubi_u(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bosubr_u(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bxsubr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  jit_gpr_t t1 = get_temp_gpr(_jit);
  jit_gpr_t t2 = get_temp_gpr(_jit);

  subr(_jit, jit_gpr_regno(t0), r0, r1);

  em_wp(_jit, _SLTI(jit_gpr_regno(t1), r1, 0));
  em_wp(_jit, _SLT(jit_gpr_regno(t2), r0, jit_gpr_regno(t0)));
  movr(_jit, r0, jit_gpr_regno(t0));
  jit_reloc_t ret = beqr(_jit, jit_gpr_regno(t1), jit_gpr_regno(t2));

  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bxsubi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bxsubr(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bxsubr_u(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  jit_gpr_t t1 = get_temp_gpr(_jit);

  subr(_jit, jit_gpr_regno(t0), r0, r1);

  em_wp(_jit, _SLTU(jit_gpr_regno(t1), r0, jit_gpr_regno(t0)));
  movr(_jit, r0, jit_gpr_regno(t0));
  jit_reloc_t ret = beqi(_jit, jit_gpr_regno(t1), 0);

  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
  return ret;
}

static jit_reloc_t
bxsubi_u(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  jit_reloc_t ret = bxsubr_u(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
  return ret;
}


/*
 * Transfer operations
 */
static void
movr(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  if (r0 != r1)
    em_wp(_jit, _MV(r0, r1));
}


static int
count_trailing_zeros(uint64_t x)
{
  if(x == 0)
    return 64;
  int count = 0;
  while((x & 0x1) == 0){
    x >>= 1;
    count++;
  }
  return count;
}

static void
movi(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  int32_t srcreg = jit_gpr_regno(_ZERO);

  if (simm32_p(i0)){
    int32_t hi = (int32_t)(((i0 + 0x800) >> 12) & 0xFFFFF) << 12 >> 12;
    int32_t lo = (int32_t)i0<<20>>20;

    if(hi){
      em_wp(_jit, _LUI(r0, hi));
      srcreg = r0;
    }

    if(lo || hi == 0){
#if __WORDSIZE == 64
      em_wp(_jit, _ADDIW(r0, srcreg, lo));
#elif __WORDSIZE == 32
      em_wp(_jit, _ADDI(r0, srcreg, lo));
#endif
    }

  } else {
    // 64 bits: load in various steps
    int64_t lo12 = i0 << 52 >> 52;
    int64_t hi52 = (i0 + 0x800) >> 12;
    int shift_amount = 12 + count_trailing_zeros((uint64_t) hi52);
    hi52 = (hi52 >> (shift_amount - 12)) << shift_amount >> shift_amount;
    movi(_jit, r0, hi52); // Recurse
    em_wp(_jit, _SLLI(r0, r0, shift_amount));
    if (lo12) {
      em_wp(_jit, _ADDI(r0, r0, lo12));
    }
  }
}

typedef union{
  struct{
    instr_t auipc;
    instr_t load;        // `ld` in RV64 and `lw` in RV32
  } inst;
  uint64_t l;
} load_from_pool_t;

static uint64_t
patch_load_from_pool(uint64_t instrs, int32_t off){

  load_from_pool_t out, in;
  int32_t hi = (int32_t)(((off + 0x800) >> 12) & 0xFFFFF) << 12 >> 12;
  int32_t lo = (int32_t)off<<20>>20;
  in.l = instrs;
  out.inst.auipc.w = _AUIPC(in.inst.auipc.U.rd, hi);
  out.inst.load.w  = Itype(in.inst.load.I.opcode, // `ld` or `lw`
                           in.inst.load.I.rd,
                           in.inst.load.I.funct3,
                           in.inst.load.I.rs1,
                           lo);
  return out.l;
}

static jit_reloc_t
emit_load_from_pool(jit_state_t *_jit, uint64_t insts)
{
  while (1) {
    uint8_t *pc_base = _jit->pc.uc;   // Offset is from current PC
    int32_t off = (_jit->pc.uc - pc_base);
    jit_reloc_t ret =
      jit_reloc (_jit, JIT_RELOC_LOAD_FROM_POOL, 0, _jit->pc.uc, pc_base, 0);
    uint8_t load_from_pool_width = 32;
    if (add_pending_literal(_jit, ret, load_from_pool_width)) {
      emit_u64(_jit, patch_load_from_pool(insts, off));
      return ret;
    }
  }
}
static jit_reloc_t
movi_from_pool(jit_state_t *_jit, int32_t r0)
{
  load_from_pool_t insts;
  insts.inst.auipc.w = _AUIPC(r0, 0);
#if __WORDSIZE == 64
  insts.inst.load.w  = _LD(r0, r0, 0);
#elif __WORDSIZE == 32
  insts.inst.load.w  = _LW(r0, r0, 0);
#endif
  return emit_load_from_pool(_jit, insts.l);
}
static jit_reloc_t
mov_addr(jit_state_t *_jit, int32_t r0)
{
  return movi_from_pool(_jit, r0);
}


static void
extr_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int rot = __WORDSIZE - 8;
  lshi(_jit, r0, r1, rot);
  rshi(_jit, r0, r0, rot);
}

static void
extr_uc(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int rot = __WORDSIZE - 8;
  lshi(_jit, r0, r1, rot);
  rshi_u(_jit, r0, r0, rot);
}

static void
extr_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int rot = __WORDSIZE - 16;
  lshi(_jit, r0, r1, rot);
  rshi(_jit, r0, r0, rot);
}

static void
extr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int rot = __WORDSIZE - 16;
  lshi(_jit, r0, r1, rot);
  rshi_u(_jit, r0, r0, rot);
}

#  if __WORDSIZE == 64
static void
extr_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int rot = __WORDSIZE - 32;
  lshi(_jit, r0, r1, rot);
  rshi(_jit, r0, r0, rot);
}
static void
extr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  int rot = __WORDSIZE - 32;
  lshi(_jit, r0, r1, rot);
  rshi_u(_jit, r0, r0, rot);
}
#endif

/*
 * Store operations
 */
static void
str_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _SB(r0, r1, 0));
}
static void
str_uc(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _SB(r0, r1, 0));
}
static void
str_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _SH(r0, r1, 0));
}
static void
str_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _SW(r0, r1, 0));
}
#if __WORDSIZE == 64
static void
str_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _SD(r0, r1, 0));
}
#endif

static void
sti_c(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  str_c(_jit, jit_gpr_regno(t0), r0);
  unget_temp_gpr(_jit);
}

static void
sti_s(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  str_s(_jit, jit_gpr_regno(t0), r0);
  unget_temp_gpr(_jit);
}

static void
sti_i(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  str_i(_jit, jit_gpr_regno(t0), r0);
  unget_temp_gpr(_jit);
}

#if __WORDSIZE == 64
static void
sti_l(jit_state_t *_jit, jit_word_t i0, int32_t r0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  str_l(_jit, jit_gpr_regno(t0), r0);
  unget_temp_gpr(_jit);
}
#endif

static void
stxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r0, r1);
  str_c(_jit, jit_gpr_regno(t0), r2);
  unget_temp_gpr(_jit);
}

static void
stxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r0, r1);
  str_s(_jit, jit_gpr_regno(t0), r2);
  unget_temp_gpr(_jit);
}

static void
stxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r0, r1);
  str_i(_jit, jit_gpr_regno(t0), r2);
  unget_temp_gpr(_jit);
}

# if __WORDSIZE == 64
static void
stxr_l(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r0, r1);
  str_l(_jit, jit_gpr_regno(t0), r2);
  unget_temp_gpr(_jit);
}
#endif

static void
stxi_c(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (simm12_p(i0))
    em_wp(_jit, _SB(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r0, i0);
    str_c(_jit, jit_gpr_regno(t0), r1);
    unget_temp_gpr(_jit);
  }
}


static void
stxi_s(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (simm12_p(i0))
    em_wp(_jit, _SH(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r0, i0);
    str_s(_jit, jit_gpr_regno(t0), r1);
    unget_temp_gpr(_jit);
  }
}


static void
stxi_i(jit_state_t *_jit, jit_word_t i0, int32_t r0, int32_t r1)
{
  if (simm12_p(i0))
    em_wp(_jit, _SW(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r0, i0);
    str_i(_jit, jit_gpr_regno(t0), r1);
    unget_temp_gpr(_jit);
  }
}

# if __WORDSIZE == 64
static void
stxi_l(jit_state_t *_jit,jit_word_t i0,int32_t r0,int32_t r1)
{
  if (simm12_p(i0))
    em_wp(_jit, _SD(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r0, i0);
    str_l(_jit, jit_gpr_regno(t0), r1);
    unget_temp_gpr(_jit);
  }
}
# endif


/*
 * Load operations
 */
static void
ldr_c(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _LB(r0, r1, 0));
}

static void
ldr_uc(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _LBU(r0, r1, 0));
}

static void
ldr_s(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _LH(r0, r1, 0));
}

static void
ldr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _LHU(r0, r1, 0));
}

static void
ldr_i(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _LW(r0, r1, 0));
}

# if __WORDSIZE == 64
static void
ldr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _LWU(r0, r1, 0));
}

static void
ldr_l(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  em_wp(_jit, _LD(r0, r1, 0));
}
# endif


static void
ldi_c(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  ldr_c(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
ldi_uc(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  ldr_uc(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
ldi_s(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  ldr_s(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
ldi_us(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  ldr_us(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}


static void
ldi_i(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  ldr_i(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

#  if __WORDSIZE == 64
static void
ldi_ui(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  ldr_ui(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}

static void
ldi_l(jit_state_t *_jit, int32_t r0, jit_word_t i0)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  movi(_jit, jit_gpr_regno(t0), i0);
  ldr_l(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
#endif




static void
ldxr_c(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r1, r2);
  ldr_c(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
ldxr_uc(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r1, r2);
  ldr_uc(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
ldxr_s(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r1, r2);
  ldr_s(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
ldxr_us(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r1, r2);
  ldr_us(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
ldxr_i(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r1, r2);
  ldr_i(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
#  if __WORDSIZE == 64
static void
ldxr_ui(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r1, r2);
  ldr_ui(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
static void
ldxr_l(jit_state_t *_jit, int32_t r0, int32_t r1, int32_t r2)
{
  jit_gpr_t t0 = get_temp_gpr(_jit);
  addr(_jit, jit_gpr_regno(t0), r1, r2);
  ldr_l(_jit, r0, jit_gpr_regno(t0));
  unget_temp_gpr(_jit);
}
#endif




static void
ldxi_c(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0))
    em_wp(_jit, _LB(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    ldr_c(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}
static void
ldxi_uc(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0))
    em_wp(_jit, _LBU(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    ldr_uc(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}
static void
ldxi_us(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0))
    em_wp(_jit, _LHU(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    ldr_us(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}
static void
ldxi_s(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0))
    em_wp(_jit, _LH(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    ldr_s(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}
static void
ldxi_i(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0))
    em_wp(_jit, _LW(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    ldr_i(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}
#  if __WORDSIZE == 64
static void
ldxi_ui(jit_state_t *_jit, int32_t r0, int32_t r1, jit_word_t i0)
{
  if (simm12_p(i0))
    em_wp(_jit, _LWU(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    ldr_ui(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}
static void
ldxi_l(jit_state_t *_jit,int32_t r0,int32_t r1,jit_word_t i0)
{
  if (simm12_p(i0))
    em_wp(_jit, _LD(r0, r1, i0));
  else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    addi(_jit, jit_gpr_regno(t0), r1, i0);
    ldr_l(_jit, r0, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}
#endif


/*
 * Argument management
 */

// static void
// pushr(jit_state_t *_jit, int32_t r0)
// {
// #if __WORDSIZE == 64
//   addi(jit_gpr_regno(_SP), -8);
//   em_wp(_SD(r0, jit_gpr_regno(_SP), 0));
// #elif __WORDSIZE == 32
//   addi(jit_gpr_regno(_SP), -4);
//   em_wp(_SW(r0, jit_gpr_regno(_SP), 0));
// #endif
// }
// static void
// popr(jit_state_t *_jit, int32_t r0)
// {
// #if __WORDSIZE == 64
//   em_wp(_jit, _LD(r0, jit_gpr_regno(_SP), 0));
//   addi(jit_gpr_regno(_SP), 8);
// #elif __WORDSIZE == 32
//   em_wp(_jit, _LW(r0, jit_gpr_regno(_SP), 0));
//   addi(jit_gpr_regno(_SP), 4);
// #endif
// }

static void
ret(jit_state_t *_jit)
{
  em_wp(_jit, _RET());
}

static void
retr(jit_state_t *_jit, int32_t r0)
{
  movr(_jit, jit_gpr_regno(_A0), r0);
  ret(_jit);
}

static void
reti(jit_state_t *_jit, jit_word_t i0)
{
  movi(_jit, jit_gpr_regno(_A0), i0);
  ret(_jit);
}

static void
retval_c(jit_state_t *_jit, int32_t r0)
{
  extr_c(_jit, r0, jit_gpr_regno(_A0));
}

static void
retval_uc(jit_state_t *_jit, int32_t r0)
{
  extr_uc(_jit, r0, jit_gpr_regno(_A0));
}

static void
retval_s(jit_state_t *_jit, int32_t r0)
{
  extr_s(_jit, r0, jit_gpr_regno(_A0));
}

static void
retval_us(jit_state_t *_jit, int32_t r0)
{
  extr_us(_jit, r0, jit_gpr_regno(_A0));
}

static void
retval_i(jit_state_t *_jit, int32_t r0)
{
  extr_i(_jit, r0, jit_gpr_regno(_A0));
}

#  if __WORDSIZE == 64
static void
retval_ui(jit_state_t *_jit, int32_t r0)
{
  extr_ui(_jit, r0, jit_gpr_regno(_A0));
}

static void
retval_l(jit_state_t *_jit, int32_t r0)
{
  movr(_jit, r0, jit_gpr_regno(_A0));
}
#endif

/*
 * Jump and return instructions
 */
static uint32_t
patch_jump(uint32_t inst, int32_t offset)
{
  instr_t i;
  i.w = inst;
  return Jtype(i.J.opcode, i.J.rd, offset);
}
static jit_reloc_t
emit_jump(jit_state_t *_jit, uint32_t inst)
{
  while (1) {
    uint8_t *pc_base = _jit->pc.uc;   // Offset is from current PC
    int32_t off = (uint8_t*)jit_address(_jit) - pc_base;
    jit_reloc_t ret =
      jit_reloc (_jit, JIT_RELOC_JMP_WITH_VENEER, 0, _jit->pc.uc, pc_base, 0);
    uint8_t jump_width = 20;
    if (add_pending_literal(_jit, ret, jump_width - 1)) {
      em_wp(_jit, patch_jump(inst, off));
      return ret;
    }
  }
}

static void
callr(jit_state_t *_jit, int32_t r0)
{
  em_wp(_jit, _JALR(jit_gpr_regno(_RA), r0, 0));
}

static void
calli(jit_state_t *_jit, jit_word_t i0)
{
  jit_word_t jumpoffset = i0 - (jit_word_t)(_jit->pc.uc);
  if (simm20_p(jumpoffset)){
    em_wp(_jit, _JAL(jit_gpr_regno(_RA), jumpoffset));
  } else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(t0), i0);
    callr(_jit, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}

static void
jmpi_with_link(jit_state_t *_jit, jit_word_t i0)
{
  calli(_jit, i0);
}

static void
pop_link_register(jit_state_t *_jit)
{
}

static void
push_link_register(jit_state_t *_jit)
{
}

static void
jmpr(jit_state_t *_jit, int32_t r0)
{
  em_wp(_jit, _JALR(jit_gpr_regno(_ZERO), r0, 0));
}

static void
jmpi(jit_state_t *_jit, jit_word_t i0)
{
  jit_word_t jumpoffset = i0 - (jit_word_t)(_jit->pc.uc);
  if (simm20_p(jumpoffset)){
    em_wp(_jit, _JAL(jit_gpr_regno(_ZERO), jumpoffset));
  } else {
    jit_gpr_t t0 = get_temp_gpr(_jit);
    movi(_jit, jit_gpr_regno(t0), i0);
    jmpr(_jit, jit_gpr_regno(t0));
    unget_temp_gpr(_jit);
  }
}

static jit_reloc_t
jmp(jit_state_t *_jit)
{
  return emit_jump(_jit, _JAL(jit_gpr_regno(_ZERO), 0));
}



/*
 * Atomic operations
 */

static void
ldr_atomic(jit_state_t *_jit, int32_t dst, int32_t loc)
{
  em_wp(_jit, _FENCE(0xFF));
#if __WORDSIZE == 64
  ldr_l(_jit, dst, loc);
#elif __WORDSIZE == 32
  ldr_i(_jit, dst, loc);
#endif
  em_wp(_jit, _FENCE(0xFF));
}

static void
str_atomic(jit_state_t *_jit, int32_t loc, int32_t val)
{
  em_wp(_jit, _FENCE(0xFF));
#if __WORDSIZE == 64
  str_l(_jit, loc, val);
#elif __WORDSIZE == 32
  str_i(_jit, loc, val);
#endif
  em_wp(_jit, _FENCE(0xFF));
}

static void
swap_atomic(jit_state_t *_jit, int32_t dst, int32_t loc, int32_t val)
{
#if __WORDSIZE == 64
  em_wp(_jit, _AMOSWAP_D(dst, loc, val, 1, 1));
#elif __WORDSIZE == 32
  em_wp(_jit, _AMOSWAP_W(dst, loc, val, 1, 1));
#endif
}

static void
cas_atomic(jit_state_t *_jit, int32_t dst, int32_t loc, int32_t expected,
           int32_t desired)
{
  int32_t t0 = jit_gpr_regno(get_temp_gpr(_jit));
  int32_t t1 = jit_gpr_regno(get_temp_gpr(_jit));

  void *retry = jit_address(_jit);

#if __WORDSIZE == 64
  em_wp(_jit, _LR_D(t0, loc, 0,0));
#elif __WORDSIZE == 32
  em_wp(_jit, _LR_W(t0, loc, 0,0));
#endif

  jit_reloc_t fail = bner(_jit, t0, expected);

#if __WORDSIZE == 64
  em_wp(_jit, _SC_D(t1, desired, loc, 0,0));
#elif __WORDSIZE == 32
  em_wp(_jit, _SC_W(t1, desired, loc, 0,0));
#endif

  jit_patch_there(_jit, bner(_jit, t1, jit_gpr_regno(_ZERO)), retry);

  jit_patch_here(_jit, fail);
  em_wp(_jit, _FENCE(0xFF));
  movr(_jit, dst, t0);

  unget_temp_gpr(_jit);
  unget_temp_gpr(_jit);
}


/*
 * Byte swapping operations
 * RISC-V Doesn't provide them by default.
 * There's a B extension (Standard Extension for Bit Manipulation) draft, but
 * it's not official yet:
 *     https://github.com/riscv/riscv-bitmanip
 * Meanwhile, we need to implement them in software.
 */
static void
bswapr_uany(jit_state_t *_jit, int32_t r0, int32_t r1, size_t size)
{
  jit_gpr_t tmp1 = get_temp_gpr(_jit);
  int32_t t0 = jit_gpr_regno(tmp1);

  andi(_jit, r0, r1, 0xFF);
  for(int i = 1; i < size; i++){
    lshi(_jit, r0, r0, 8);
    rshi(_jit, t0, r1, 8*i);
    andi(_jit, t0, t0, 0xFF);
    orr(_jit, r0, r0, t0);
  }
  unget_temp_gpr(_jit);
}

static void
bswapr_us(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  bswapr_uany(_jit, r0, r1, 2);
}

static void
bswapr_ui(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  bswapr_uany(_jit, r0, r1, 4);
}

#  if __WORDSIZE == 64
static void
bswapr_ul(jit_state_t *_jit, int32_t r0, int32_t r1)
{
  bswapr_uany(_jit, r0, r1, 8);
}
#endif



/*
 * Others
 * TODO
 */
static void
nop(jit_state_t *_jit, int32_t im)
{
  for (; im > 0; im -= 4)
    em_wp(_jit, _NOP());
  assert(im == 0);
}
static void
mfence(jit_state_t *_jit)
{
  // TODO: we may need it for atomic operations?
  em_wp(_jit, _FENCE(0xFF));
}

static void
breakpoint(jit_state_t *_jit)
{
  em_wp(_jit, _EBREAK());
}
