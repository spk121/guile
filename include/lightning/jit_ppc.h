/*
 * Copyright (C) 2012  Free Software Foundation, Inc.
 *
 * This is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Authors:
 *	Paulo Cesar Pereira de Andrade
 */

#ifndef _jit_ppc_h
#define _jit_ppc_h

#define JIT_HASH_CONSTS		1
#define JIT_NUM_OPERANDS	3

/*
 * Types
 */
typedef enum {
#define jit_arg_reg_p(i)	((i) >= 0 && (i) < 8)
#define jit_r(i)		(_R11 + (i))
#define jit_r_num()		3
#define jit_v(i)		(_R30 - (i))
#define jit_r_num()		17
#define jit_arg_f_reg_p(i)	((i) >= 0 && (i) < 8)
#define jit_f(i)		(_F0 + (i))
#define jit_f_num()		6
    _R0,
#define JIT_R0			_R11
#define JIT_R1			_R12
#define JIT_R2			_R13
#define JIT_R3			_R2
    _R11,	_R12,	_R13,	_R2,
#define JIT_V0			_R30
#define JIT_V1			_R29
#define JIT_V2			_R28
#define JIT_V3			_R28
#define JIT_V4			_R26
#define JIT_V5			_R25
#define JIT_V6			_R24
#define JIT_V7			_R23
#define JIT_V8			_R22
#define JIT_V9			_R21
#define JIT_V10			_R20
#define JIT_V11			_R19
#define JIT_V12			_R18
#define JIT_V13			_R17
#define JIT_V14			_R16
#define JIT_V15			_R15
#define JIT_V16			_R14
    _R14,	_R15,	_R16,	_R17,	_R18,	_R19,	_R20,	_R21,
    _R22,	_R23,	_R24,	_R25,	_R26,	_R27,	_R28,	_R29,
    _R30,
#define JIT_SP			_R1
    _R1,
#define JIT_FP			_R31
    _R31,
#define JIT_RET			_R3
#define JIT_RA0			_R3
#define JIT_RA1			_R4
#define JIT_RA2			_R5
#define JIT_RA3			_R6
#define JIT_RA4			_R7
#define JIT_RA5			_R8
#define JIT_RA6			_R9
#define JIT_RA7			_R10
    _R10,	_R9,	_R8,	_R7,	_R6,	_R5,	_R4,	_R3,
#  define JIT_F0		_F0
#  define JIT_F1		_F8
#  define JIT_F2		_F9
#  define JIT_F3		_F10
#  define JIT_F4		_F11
#  define JIT_F5		_F12
    _F0,	_F9,	_F10,	_F11,	_F12,	_F13,
#define JIT_FS0			_F14
#define JIT_FS1			_F15
#define JIT_FS2			_F16
#define JIT_FS3			_F17
#define JIT_FS4			_F18
#define JIT_FS5			_F19
#define JIT_FS6			_F20
#define JIT_FS7			_F21
#define JIT_FS8			_F22
#define JIT_FS9			_F23
#define JIT_FS10		_F24
#define JIT_FS11		_F25
#define JIT_FS12		_F26
#define JIT_FS13		_F27
#define JIT_FS14		_F28
#define JIT_FS15		_F29
#define JIT_FS16		_F30
#define JIT_FS17		_F31
    _F14,	_F15,	_F16,	_F17,	_F18,	_F19,	_F20,
    _F21,	_F22,	_F23,	_F24,	_F25,	_F26,	_F27,
    _F28,	_F29,	_F30,	_F31,
#define JIT_FRET		_F1
#define JIT_FA0			_F1
#define JIT_FA1			_F2
#define JIT_FA2			_F3
#define JIT_FA3			_F4
#define JIT_FA4			_F5
#define JIT_FA5			_F6
#define JIT_FA6			_F7
#define JIT_FA7			_F8
    _F8,	_F7,	_F6,	_F5,	_F4,	_F3,	_F2,	_F1,
    _NOREG,
#define JIT_NOREG		_NOREG
} jit_reg_t;

typedef jit_int64_t		jit_regset_t;

#endif /* _jit_ppc_h */
