/*
 * Copyright (C) 2012-2018  Free Software Foundation, Inc.
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
 *	Paulo Cesar Pereira de Andrade
 *	Andy Wingo
 */

#ifndef _jit_h
#define _jit_h

#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stddef.h>

#include "jit/endian.h"

CHOOSE_32_64(typedef int32_t jit_word_t,
             typedef int64_t jit_word_t);
CHOOSE_32_64(typedef uint32_t jit_uword_t,
             typedef uint64_t jit_uword_t);
typedef float		jit_float32_t;
typedef double		jit_float64_t;
typedef void*		jit_pointer_t;
typedef int		jit_bool_t;
/* FIXME: Make the compiler warn when confusing GPR/FPR/immediate.  */
typedef int		jit_gpr_t;
typedef int		jit_fpr_t;

typedef void*		jit_addr_t;
typedef ptrdiff_t	jit_off_t;
typedef intptr_t	jit_imm_t;
typedef uintptr_t	jit_uimm_t;
typedef struct jit_reloc *jit_reloc_t;

#if defined(__GNUC__) && (__GNUC__ >= 4)
#  define JIT_API		extern __attribute__ ((__visibility__("hidden")))
#else
#  define JIT_API		extern
#endif

#if defined(__i386__) || defined(__x86_64__)
#  include "jit/x86.h"
#elif defined(__mips__)
#  include "jit/mips.h"
#elif defined(__arm__)
#  include "jit/arm.h"
#elif defined(__ppc__) || defined(__powerpc__)
#  include "jit/ppc.h"
#elif defined(__sparc__)
#  include "jit/sparc.h"
#elif defined(__ia64__)
#  include "jit/ia64.h"
#elif defined(__hppa__)
#  include "jit/hppa.h"
#elif defined(__aarch64__)
#  include "jit/aarch64.h"
#elif defined(__s390__) || defined(__s390x__)
#  include "jit/s390.h"
#elif defined(__alpha__)
#  include "jit/alpha.h"
#endif

#define JIT_R(index)		jit_r(index)
#define JIT_V(index)		jit_v(index)
#define JIT_F(index)		jit_f(index)
#define JIT_R_NUM		jit_r_num()
#define JIT_V_NUM		jit_v_num()
#define JIT_F_NUM		jit_f_num()

#define jit_class_chk		0x02000000	/* just checking */
#define jit_class_arg		0x08000000	/* argument register */
#define jit_class_sav		0x10000000	/* callee save */
#define jit_class_gpr		0x20000000	/* general purpose */
#define jit_class_fpr		0x40000000	/* float */
#define jit_class(reg)		((reg) & 0xffff0000)
#define jit_regno(reg)		((reg) & 0x00007fff)

typedef struct jit_state	jit_state_t;
enum jit_arg_kind
{
  JIT_CALL_ARG_IMM,
  JIT_CALL_ARG_GPR,
  JIT_CALL_ARG_FPR,
  JIT_CALL_ARG_MEM
};

typedef struct jit_arg
{
  enum jit_arg_kind kind;
  union
  {
    intptr_t imm;
    jit_gpr_t gpr;
    jit_fpr_t fpr;
    struct { jit_gpr_t base; ptrdiff_t offset; } mem;
  } loc;
} jit_arg_t;

JIT_API void init_jit(void);

JIT_API jit_state_t *jit_new_state(void);
JIT_API void jit_destroy_state(jit_state_t*);

JIT_API void jit_begin(jit_state_t*, jit_addr_t, size_t);
JIT_API void jit_reset(jit_state_t*);
JIT_API jit_addr_t jit_end(jit_state_t*, size_t*);

JIT_API void jit_align(jit_state_t*, unsigned);
JIT_API void jit_allocai(jit_state_t*, size_t);
JIT_API void jit_allocar(jit_state_t*, jit_gpr_t, jit_gpr_t);

JIT_API jit_pointer_t jit_address(jit_state_t*);
JIT_API void jit_patch_here(jit_state_t*, jit_reloc_t);
JIT_API void jit_patch_there(jit_state_t*, jit_reloc_t, jit_pointer_t);

JIT_API void jit_calli(jit_state_t *, jit_pointer_t f,
                      size_t argc, const jit_arg_t *argv);
JIT_API void jit_callr(jit_state_t *, jit_gpr_t f,
                      size_t argc, const jit_arg_t *argv);
JIT_API void jit_receive(jit_state_t*, size_t argc, jit_arg_t *argv);

#define JIT_PROTO_0(stem, ret) \
  JIT_API ret jit_##stem (jit_state_t*)
#define JIT_PROTO_1(stem, ret, a) \
  JIT_API ret jit_##stem (jit_state_t*, jit_##a##_t)
#define JIT_PROTO_2(stem, ret, a, b) \
  JIT_API ret jit_##stem (jit_state_t*, jit_##a##_t, jit_##b##_t)
#define JIT_PROTO_3(stem, ret, a, b, c) \
  JIT_API ret jit_##stem (jit_state_t*, jit_##a##_t, jit_##b##_t, jit_##c##_t)
#define JIT_PROTO_4(stem, ret, a, b, c, d) \
  JIT_API ret jit_##stem (jit_state_t*, jit_##a##_t, jit_##b##_t, jit_##c##_t, jit_##d##_t)

#define JIT_PROTO_RFF__(stem) JIT_PROTO_2(stem, jit_reloc_t, fpr, fpr)
#define JIT_PROTO_RGG__(stem) JIT_PROTO_2(stem, jit_reloc_t, gpr, gpr)
#define JIT_PROTO_RG___(stem) JIT_PROTO_1(stem, jit_reloc_t, gpr)
#define JIT_PROTO_RGi__(stem) JIT_PROTO_2(stem, jit_reloc_t, gpr, imm)
#define JIT_PROTO_RGu__(stem) JIT_PROTO_2(stem, jit_reloc_t, gpr, uimm)
#define JIT_PROTO_R____(stem) JIT_PROTO_0(stem, jit_reloc_t)
#define JIT_PROTO__FFF_(stem) JIT_PROTO_3(stem, void, fpr, fpr, fpr)
#define JIT_PROTO__FF__(stem) JIT_PROTO_2(stem, void, fpr, fpr)
#define JIT_PROTO__FGG_(stem) JIT_PROTO_3(stem, void, fpr, gpr, gpr)
#define JIT_PROTO__FG__(stem) JIT_PROTO_2(stem, void, fpr, gpr)
#define JIT_PROTO__FGo_(stem) JIT_PROTO_3(stem, void, fpr, gpr, off)
#define JIT_PROTO__F___(stem) JIT_PROTO_1(stem, void, fpr)
#define JIT_PROTO__Fd__(stem) JIT_PROTO_2(stem, void, fpr, float64)
#define JIT_PROTO__Ff__(stem) JIT_PROTO_2(stem, void, fpr, float32)
#define JIT_PROTO__Fp__(stem) JIT_PROTO_2(stem, void, fpr, pointer)
#define JIT_PROTO__GF__(stem) JIT_PROTO_2(stem, void, gpr, fpr)
#define JIT_PROTO__GGF_(stem) JIT_PROTO_3(stem, void, gpr, gpr, fpr)
#define JIT_PROTO__GGGG(stem) JIT_PROTO_4(stem, void, gpr, gpr, gpr, gpr)
#define JIT_PROTO__GGG_(stem) JIT_PROTO_3(stem, void, gpr, gpr, gpr)
#define JIT_PROTO__GGGi(stem) JIT_PROTO_3(stem, void, gpr, gpr, imm)
#define JIT_PROTO__GGGu(stem) JIT_PROTO_3(stem, void, gpr, gpr, uimm)
#define JIT_PROTO__GG__(stem) JIT_PROTO_2(stem, void, gpr, gpr)
#define JIT_PROTO__GGi_(stem) JIT_PROTO_3(stem, void, gpr, gpr, imm)
#define JIT_PROTO__GGo_(stem) JIT_PROTO_3(stem, void, gpr, gpr, off)
#define JIT_PROTO__GGu_(stem) JIT_PROTO_3(stem, void, gpr, gpr, uimm)
#define JIT_PROTO__G___(stem) JIT_PROTO_1(stem, void, gpr)
#define JIT_PROTO__Gi__(stem) JIT_PROTO_2(stem, void, gpr, imm)
#define JIT_PROTO__Gp__(stem) JIT_PROTO_2(stem, void, gpr, pointer)
#define JIT_PROTO______(stem) JIT_PROTO_0(stem, void)
#define JIT_PROTO__i___(stem) JIT_PROTO_1(stem, void, imm)
#define JIT_PROTO__oGF_(stem) JIT_PROTO_3(stem, void, off, gpr, fpr)
#define JIT_PROTO__oGG_(stem) JIT_PROTO_3(stem, void, off, gpr, gpr)
#define JIT_PROTO__pF__(stem) JIT_PROTO_2(stem, void, pointer, fpr)
#define JIT_PROTO__pG__(stem) JIT_PROTO_2(stem, void, pointer, gpr)
#define JIT_PROTO__p___(stem) JIT_PROTO_1(stem, void, pointer)

#define FOR_EACH_INSTRUCTION(M)		\
          M(_GGG_, addr)		\
          M(_FFF_, addr_f)		\
          M(_FFF_, addr_d)		\
          M(_GGi_, addi)		\
          M(_GGG_, addcr)		\
          M(_GGi_, addci)		\
          M(_GGG_, addxr)		\
          M(_GGi_, addxi)		\
          M(_GGG_, subr)		\
          M(_FFF_, subr_f)		\
          M(_FFF_, subr_f)		\
          M(_GGi_, subi)		\
          M(_GGG_, subcr)		\
          M(_GGi_, subci)		\
          M(_GGG_, subxr)		\
          M(_GGi_, subxi)		\
          M(_GGG_, mulr)		\
          M(_FFF_, mulr_f)		\
          M(_FFF_, mulr_d)		\
          M(_GGi_, muli)		\
          M(_GGGG, qmulr)		\
          M(_GGGi, qmuli)		\
          M(_GGGG, qmulr_u)		\
          M(_GGGu, qmuli_u)		\
          M(_GGG_, divr)		\
          M(_FFF_, divr_f)		\
          M(_FFF_, divr_d)		\
          M(_GGi_, divi)		\
          M(_GGG_, divr_u)		\
          M(_GGu_, divi_u)		\
          M(_GGGG, qdivr)		\
          M(_GGGi, qdivi)		\
          M(_GGGG, qdivr_u)		\
          M(_GGGu, qdivi_u)		\
          M(_GGG_, remr)		\
          M(_GGi_, remi)		\
          M(_GGG_, remr_u)		\
          M(_GGu_, remi_u)		\
  					\
          M(_GGG_, andr)		\
          M(_GGu_, andi)		\
          M(_GGG_, orr)			\
          M(_GGu_, ori)			\
          M(_GGG_, xorr)		\
          M(_GGu_, xori)		\
  					\
          M(_GGG_, lshr)		\
          M(_GGu_, lshi)		\
          M(_GGG_, rshr)		\
          M(_GGu_, rshi)		\
          M(_GGG_, rshr_u)		\
          M(_GGu_, rshi_u)		\
  					\
          M(_GG__, negr)		\
          M(_GG__, comr)		\
  					\
          M(_GG__, movr)		\
          M(_Gi__, movi)		\
          M(RG___, mov_addr)		\
          M(_GG__, extr_c)		\
          M(_GG__, extr_uc)		\
          M(_GG__, extr_s)		\
          M(_GG__, extr_us)		\
  WHEN_64(M(_GG__, extr_i))		\
  WHEN_64(M(_GG__, extr_ui))		\
  					\
          M(_GG__, bswapr_us)		\
          M(_GG__, bswapr_ui)		\
  WHEN_64(M(_GG__, bswapr_ul))		\
  					\
          M(_GG__, ldr_c)		\
          M(_Gp__, ldi_c)		\
          M(_GG__, ldr_uc)		\
          M(_Gp__, ldi_uc)		\
          M(_GG__, ldr_s)		\
          M(_Gp__, ldi_s)		\
          M(_GG__, ldr_us)		\
          M(_Gp__, ldi_us)		\
          M(_GG__, ldr_i)		\
          M(_Gp__, ldi_i)		\
  WHEN_64(M(_GG__, ldr_ui))		\
  WHEN_64(M(_Gp__, ldi_ui))		\
  WHEN_64(M(_GG__, ldr_l))		\
  WHEN_64(M(_Gp__, ldi_l))		\
          M(_FG__, ldr_f)		\
          M(_Fp__, ldi_f)		\
          M(_FG__, ldr_d)		\
          M(_Fp__, ldi_d)		\
  					\
          M(_GGG_, ldxr_c)		\
          M(_GGo_, ldxi_c)		\
          M(_GGG_, ldxr_uc)		\
          M(_GGo_, ldxi_uc)		\
          M(_GGG_, ldxr_s)		\
          M(_GGo_, ldxi_s)		\
          M(_GGG_, ldxr_us)		\
          M(_GGo_, ldxi_us)		\
          M(_GGG_, ldxr_i)		\
          M(_GGo_, ldxi_i)		\
  WHEN_64(M(_GGG_, ldxr_ui))		\
  WHEN_64(M(_GGo_, ldxi_ui))		\
  WHEN_64(M(_GGG_, ldxr_l))		\
  WHEN_64(M(_GGo_, ldxi_l))		\
          M(_FGG_, ldxr_f)		\
          M(_FGo_, ldxi_f)		\
          M(_FGG_, ldxr_d)		\
          M(_FGo_, ldxi_d)		\
  					\
          M(_GG__, str_c)		\
          M(_pG__, sti_c)		\
          M(_GG__, str_s)		\
          M(_pG__, sti_s)		\
          M(_GG__, str_i)		\
          M(_pG__, sti_i)		\
  WHEN_64(M(_GG__, str_l))		\
  WHEN_64(M(_pG__, sti_l))		\
          M(_GF__, str_f)		\
          M(_pF__, sti_f)		\
          M(_GF__, str_d)		\
          M(_pF__, sti_d)		\
  					\
          M(_GGG_, stxr_c)		\
          M(_oGG_, stxi_c)		\
          M(_GGG_, stxr_s)		\
          M(_oGG_, stxi_s)		\
          M(_GGG_, stxr_i)		\
          M(_oGG_, stxi_i)		\
  WHEN_64(M(_GGG_, stxr_l))		\
  WHEN_64(M(_oGG_, stxi_l))		\
          M(_GGF_, stxr_f)		\
          M(_oGF_, stxi_f)		\
          M(_GGF_, stxr_d)		\
          M(_oGF_, stxi_d)		\
  					\
          M(RGG__, bltr)		\
          M(RFF__, bltr_f)		\
          M(RFF__, bltr_d)		\
          M(RGi__, blti)		\
          M(RGG__, bltr_u)		\
          M(RGu__, blti_u)		\
          M(RGG__, bler)		\
          M(RFF__, bler_f)		\
          M(RFF__, bler_d)		\
          M(RGi__, blei)		\
          M(RGG__, bler_u)		\
          M(RGu__, blei_u)		\
          M(RGG__, beqr)		\
          M(RFF__, beqr_f)		\
          M(RFF__, beqr_d)		\
          M(RGi__, beqi)		\
          M(RGG__, bger)		\
          M(RFF__, bger_f)		\
          M(RFF__, bger_d)		\
          M(RGi__, bgei)		\
          M(RGG__, bger_u)		\
          M(RGu__, bgei_u)		\
          M(RGG__, bgtr)		\
          M(RFF__, bgtr_f)		\
          M(RFF__, bgtr_d)		\
          M(RGi__, bgti)		\
          M(RGG__, bgtr_u)		\
          M(RGu__, bgti_u)		\
          M(RGG__, bner)		\
          M(RFF__, bner_f)		\
          M(RFF__, bner_d)		\
          M(RGi__, bnei)		\
  					\
          M(RFF__, bunltr_f)		\
          M(RFF__, bunltr_d)		\
          M(RFF__, bunler_f)		\
          M(RFF__, bunler_d)		\
          M(RFF__, buneqr_f)		\
          M(RFF__, buneqr_d)		\
          M(RFF__, bunger_f)		\
          M(RFF__, bunger_d)		\
          M(RFF__, bungtr_f)		\
          M(RFF__, bungtr_d)		\
          M(RFF__, bltgtr_f)		\
          M(RFF__, bltgtr_d)		\
          M(RFF__, bordr_f)		\
          M(RFF__, bordr_d)		\
          M(RFF__, bunordr_f)		\
          M(RFF__, bunordr_d)		\
  					\
          M(RGG__, bmsr)		\
          M(RGu__, bmsi)		\
          M(RGG__, bmcr)		\
          M(RGu__, bmci)		\
  					\
          M(RGG__, boaddr)		\
          M(RGi__, boaddi)		\
          M(RGG__, boaddr_u)		\
          M(RGu__, boaddi_u)		\
          M(RGG__, bxaddr)		\
          M(RGi__, bxaddi)		\
          M(RGG__, bxaddr_u)		\
          M(RGu__, bxaddi_u)		\
          M(RGG__, bosubr)		\
          M(RGi__, bosubi)		\
          M(RGG__, bosubr_u)		\
          M(RGu__, bosubi_u)		\
          M(RGG__, bxsubr)		\
          M(RGi__, bxsubi)		\
          M(RGG__, bxsubr_u)		\
          M(RGu__, bxsubi_u)		\
  					\
          M(_G___, jmpr)		\
          M(_p___, jmpi)		\
          M(R____, jmp)			\
  					\
          M(_G___, pushr)		\
          M(_F___, pushr_d)		\
          M(_G___, popr)		\
          M(_F___, popr_d)		\
					\
          M(_____, ret)			\
          M(_G___, retr)		\
          M(_F___, retr_f)		\
          M(_F___, retr_d)		\
          M(_i___, reti)		\
          M(_G___, retval_c)		\
          M(_G___, retval_uc)		\
          M(_G___, retval_s)		\
          M(_G___, retval_us)		\
          M(_G___, retval_i)		\
  WHEN_64(M(_G___, retval_ui))		\
  WHEN_64(M(_G___, retval_l))		\
          M(_F___, retval_f)		\
          M(_F___, retval_d)		\
  					\
          M(_FF__, negr_f)		\
          M(_FF__, negr_d)		\
          M(_FF__, absr_f)		\
          M(_FF__, absr_d)		\
          M(_FF__, sqrtr_f)		\
          M(_FF__, sqrtr_d)		\
  					\
          M(_GF__, truncr_f_i)		\
          M(_FG__, extr_f)		\
          M(_FG__, extr_d)		\
          M(_FF__, extr_d_f)		\
          M(_FF__, extr_f_d)		\
          M(_FF__, movr_f)		\
          M(_FF__, movr_d)		\
          M(_Ff__, movi_f)		\
          M(_Fd__, movi_d)		\
          M(_GF__, truncr_d_i)		\
  WHEN_64(M(_GF__, truncr_f_l))		\
  WHEN_64(M(_GF__, truncr_d_l))		\
          /* EOL */

#define DECLARE_INSTRUCTION(kind, stem) JIT_PROTO_##kind(stem);
FOR_EACH_INSTRUCTION(DECLARE_INSTRUCTION)
#undef DECLARE_INSTRUCTION

#if __WORDSIZE == 32
#  define jit_ldr(u,v)		jit_ldr_i(u,v)
#  define jit_ldi(u,v)		jit_ldi_i(u,v)
#  define jit_ldxr(u,v,w)	jit_ldxr_i(u,v,w)
#  define jit_ldxi(u,v,w)	jit_ldxi_i(u,v,w)
#  define jit_str(u,v)		jit_str_i(u,v)
#  define jit_sti(u,v)		jit_sti_i(u,v)
#  define jit_stxr(u,v,w)	jit_stxr_i(u,v,w)
#  define jit_stxi(u,v,w)	jit_stxi_i(u,v,w)
#  define jit_retval(u)		jit_retval_i(u)
#  define jit_bswapr(u,v)	jit_bswapr_ui(u,v)
#  define jit_truncr_d(u,v)	jit_truncr_d_i(u,v)
#  define jit_truncr_f(u,v)	jit_truncr_f_i(u,v)
#else
#  define jit_ldr(u,v)		jit_ldr_l(u,v)
#  define jit_ldi(u,v)		jit_ldi_l(u,v)
#  define jit_ldxr(u,v,w)	jit_ldxr_l(u,v,w)
#  define jit_ldxi(u,v,w)	jit_ldxi_l(u,v,w)
#  define jit_str(u,v)		jit_str_l(u,v)
#  define jit_sti(u,v)		jit_sti_l(u,v)
#  define jit_stxr(u,v,w)	jit_stxr_l(u,v,w)
#  define jit_stxi(u,v,w)	jit_stxi_l(u,v,w)
#  define jit_retval(u)		jit_retval_l(u)
#  define jit_bswapr(u,v)	jit_bswapr_ul(u,v)
#  define jit_truncr_d(u,v)	jit_truncr_d_l(u,v)
#  define jit_truncr_f(u,v)	jit_truncr_f_l(u,v)
#endif

#endif /* _jit_h */
