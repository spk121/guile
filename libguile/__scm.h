#ifndef SCM___SCM_H
#define SCM___SCM_H

/* Copyright (C) 1995-1996,1998-2003,2006-2013,2018
 *   Free Software Foundation, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 3 of
 * the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA
 */



/**********************************************************************
 This file is Guile's central public header.

 When included by other files, this file should preceed any include
 other than __scm.h.

 Under *NO* circumstances should new items be added to the global
 namespace (via adding #define, typedef, or similar to this file) with
 generic names.  This usually means that any new names should be
 prefixed by either SCM_ or GUILE_.  i.e. do *not* #define HAVE_FOO or
 SIZEOF_BAR.  See configure.in, gen-scmconfig.h.in, and
 gen-scmconfig.c for examples of how to properly handle this issue.
 The main documentation is in gen-scmconfig.c.
 **********************************************************************/

#include <stdint.h>

/* What did the configure script discover about the outside world?  */
#include "libguile/scmconfig.h"



/* {Compiler hints}
 *
 * The following macros are used to provide additional information for the
 * compiler, which may help to do better error checking and code
 * optimization.  A second benefit of these macros is, that they also provide
 * additional information to the developers.
 */

/* The macro SCM_NORETURN indicates that a function will never return.
 * Examples:
 *   1) int foo (char arg) SCM_NORETURN;
 */
#ifdef __GNUC__
#define SCM_NORETURN __attribute__ ((__noreturn__))
#else
#define SCM_NORETURN
#endif

/* The macro SCM_UNUSED indicates that a function, function argument or
 * variable may potentially be unused.
 * Examples:
 *   1) static int unused_function (char arg) SCM_UNUSED;
 *   2) int foo (char unused_argument SCM_UNUSED);
 *   3) int unused_variable SCM_UNUSED;
 */
#ifdef __GNUC__
#define SCM_UNUSED __attribute__ ((unused))
#else
#define SCM_UNUSED
#endif


/* The SCM_EXPECT macros provide branch prediction hints to the compiler.  To
 * use only in places where the result of the expression under "normal"
 * circumstances is known.  */
#ifdef __GNUC__
# define SCM_EXPECT    __builtin_expect
#else
# define SCM_EXPECT(_expr, _value) (_expr)
#endif

#define SCM_LIKELY(_expr)    SCM_EXPECT ((_expr), 1)
#define SCM_UNLIKELY(_expr)  SCM_EXPECT ((_expr), 0)

/* The SCM_INTERNAL macro makes it possible to explicitly declare a function
 * as having "internal" linkage.  However our current tack on this problem is
 * to use GCC 4's -fvisibility=hidden, making functions internal by default,
 * and then SCM_API marks them for export. */
#define SCM_INTERNAL  extern

/* The SCM_DEPRECATED macro is used in declarations of deprecated functions
 * or variables.  Defining `SCM_BUILDING_DEPRECATED_CODE' allows deprecated
 * functions to be implemented in terms of deprecated functions, and allows
 * deprecated functions to be referred to by `scm_c_define_gsubr ()'.  */
#if !defined (SCM_BUILDING_DEPRECATED_CODE) && defined __GNUC__
# define SCM_DEPRECATED  SCM_API __attribute__ ((__deprecated__))
#else
# define SCM_DEPRECATED  SCM_API
#endif

/* The SCM_ALIGNED macro, when defined, can be used to instruct the compiler
 * to honor the given alignment constraint.  */
/* Sun Studio supports alignment since Sun Studio 12 */
#if defined __GNUC__ || (defined( __SUNPRO_C ) && (__SUNPRO_C - 0 >= 0x590))
# define SCM_ALIGNED(x)  __attribute__ ((aligned (x)))
#elif defined __INTEL_COMPILER
# define SCM_ALIGNED(x)  __declspec (align (x))
#else
/* Don't know how to align things.  */
# undef SCM_ALIGNED
#endif

/* The SCM_MALLOC macro can be used in function declarations to tell the
 * compiler that a function may be treated as if any non-NULL pointer it returns
 * cannot alias any other pointer valid when the function returns.  */
#ifdef __GNUC__
# define SCM_MALLOC  __attribute__ ((__malloc__))
#else
# define SCM_MALLOC
#endif



/* SCM_API is a macro prepended to all function and data definitions
   which should be exported from libguile. */

#if defined BUILDING_LIBGUILE && defined HAVE_VISIBILITY
# define SCM_API extern __attribute__((__visibility__("default")))
#elif defined BUILDING_LIBGUILE && defined _MSC_VER
# define SCM_API __declspec(dllexport) extern
#elif defined _MSC_VER
# define SCM_API __declspec(dllimport) extern
#else
# define SCM_API extern
#endif



/* {Debugging Options}
 *
 * These compile time options determine whether to include code that is only
 * useful for debugging guile itself or C level extensions to guile.  The
 * common prefix for all option macros of this kind is "SCM_DEBUG_".  It is
 * guaranteed that a macro named SCM_DEBUG_XXX is always defined (typically to
 * either 0 or 1), i. e. there is no need to test for the undefined case.
 * This allows to use these definitions comfortably within code, as in the
 * following example:
 *   #define FOO do { if (SCM_DEBUG_XXX) bar(); else baz(); } while (0)
 * Any sane compiler will remove the unused branch without any performance
 * penalty for the resulting code.
 *
 * Note:  Some SCM_DEBUG_XXX options are not settable at configure time.
 * To change the value of such options you will have to edit this header
 * file or give suitable options to make, like:
 *   make all CFLAGS="-DSCM_DEBUG_XXX=1 ..."
 */


/* The value of SCM_DEBUG determines the default for most of the not yet
 * defined debugging options.  This allows, for example, to enable most of the
 * debugging options by simply defining SCM_DEBUG as 1.
 */
#ifndef SCM_DEBUG
#define SCM_DEBUG 0
#endif

/* If SCM_DEBUG_PAIR_ACCESSES is set to 1, accesses to cons cells will be
 * exhaustively checked.  Note:  If this option is enabled, guile will run
 * slower than normally.
 */
#ifndef SCM_DEBUG_PAIR_ACCESSES
#define SCM_DEBUG_PAIR_ACCESSES SCM_DEBUG
#endif

/* If SCM_DEBUG_REST_ARGUMENT is set to 1, functions that take rest arguments
 * will check whether the rest arguments are actually passed as a proper list.
 * Otherwise, if SCM_DEBUG_REST_ARGUMENT is 0, functions that take rest
 * arguments will take it for granted that these are passed as a proper list.
 */
#ifndef SCM_DEBUG_REST_ARGUMENT
#define SCM_DEBUG_REST_ARGUMENT SCM_DEBUG
#endif

/* The macro SCM_DEBUG_TYPING_STRICTNESS indicates what level of type checking
 * shall be performed with respect to the use of the SCM datatype.  The macro
 * may be defined to one of the values 0, 1 and 2.
 *
 * A value of 0 means that there will be no compile time type checking, since
 * the SCM datatype will be declared as an integral type.  This setting should
 * only be used on systems, where casting from integral types to pointers may
 * lead to loss of bit information.
 *
 * A value of 1 means that there will an intermediate level of compile time
 * type checking, since the SCM datatype will be declared as a pointer to an
 * undefined struct.  This setting is the default, since it does not cost
 * anything in terms of performance or code size.
 *
 * A value of 2 provides a maximum level of compile time type checking since
 * the SCM datatype will be declared as a struct.  This setting should be used
 * for _compile time_ type checking only, since the compiled result is likely
 * to be quite inefficient.  The right way to make use of this option is to do
 * a 'make clean; make CFLAGS=-DSCM_DEBUG_TYPING_STRICTNESS=2', fix your
 * errors, and then do 'make clean; make'.
 */
#ifndef SCM_DEBUG_TYPING_STRICTNESS
#define SCM_DEBUG_TYPING_STRICTNESS 1
#endif



/* {Feature Options}
 *
 * These compile time options determine whether code for certain features
 * should be compiled into guile.  The common prefix for all option macros
 * of this kind is "SCM_ENABLE_".  It is guaranteed that a macro named
 * SCM_ENABLE_XXX is defined to be either 0 or 1, i. e. there is no need to
 * test for the undefined case.  This allows to use these definitions
 * comfortably within code, as in the following example:
 *   #define FOO do { if (SCM_ENABLE_XXX) bar(); else baz(); } while (0)
 * Any sane compiler will remove the unused branch without any performance
 * penalty for the resulting code.
 *
 * Note:  Some SCM_ENABLE_XXX options are not settable at configure time.
 * To change the value of such options you will have to edit this header
 * file or give suitable options to make, like:
 *   make all CFLAGS="-DSCM_ENABLE_XXX=1 ..."
 */

/* If SCM_ENABLE_DEPRECATED is set to 1, deprecated code will be included in
 * guile, as well as some functions to issue run-time warnings about uses of
 * deprecated functions.
 */
#ifndef SCM_ENABLE_DEPRECATED
#define SCM_ENABLE_DEPRECATED 0
#endif



/* {Architecture and compiler properties}
 *
 * Guile as of today can only work on systems which fulfill at least the
 * following requirements:
 *
 * - scm_t_bits and SCM variables have at least 32 bits.
 *   Guile's type system is based on this assumption.
 *
 * - sizeof (scm_t_bits) >= sizeof (void*) and sizeof (SCM) >= sizeof (void*)
 *   Guile's type system is based on this assumption, since it must be
 *   possible to store pointers to cells on the heap in scm_t_bits and SCM
 *   variables.
 *
 * - sizeof (scm_t_bits) >= 4 and sizeof (scm_t_bits) is a power of 2.
 *   Guile's type system is based on this assumption.  In particular, it is
 *   assumed that cells, i. e. pairs of scm_t_bits variables, are eight
 *   character aligned.  This is because three bits of a scm_t_bits variable
 *   that is holding a pointer to a cell on the heap must be available for
 *   storing type data.
 *
 * - sizeof (scm_t_bits) <= sizeof (void*) and sizeof (SCM) <= sizeof (void*)
 *   In some parts of guile, scm_t_bits and SCM variables are passed to
 *   functions as void* arguments.  Together with the requirement above, this
 *   requires a one-to-one correspondence between the size of a void* and the
 *   sizes of scm_t_bits and SCM variables.
 *
 * - numbers are encoded using two's complement.
 *   The implementation of the bitwise scheme level operations is based on
 *   this assumption.
 *
 * - ... add more
 */

#ifdef CHAR_BIT
# define SCM_CHAR_BIT CHAR_BIT
#else
# define SCM_CHAR_BIT 8
#endif

#ifdef LONG_BIT
# define SCM_LONG_BIT LONG_BIT
#else
# define SCM_LONG_BIT (SCM_SIZEOF_LONG * 8)
#endif



#include "libguile/tags.h"


/* The type of subrs, i.e., Scheme procedures implemented in C.  Empty
   function declarators are used internally for pointers to functions of
   any arity.  However, these are equivalent to `(void)' in C++, are
   obsolescent as of C99, and trigger `strict-prototypes' GCC warnings
   (bug #23681).  */

#ifdef BUILDING_LIBGUILE
typedef SCM (* scm_t_subr) ();
#else
typedef void *scm_t_subr;
#endif



typedef struct scm_dynamic_state scm_t_dynamic_state;




/* If stack is not longword aligned then
 */

typedef long SCM_STACKITEM;

/* Cast pointer through (void *) in order to avoid compiler warnings
   when strict aliasing is enabled */
#define SCM_STACK_PTR(ptr) ((SCM_STACKITEM *) (void *) (ptr))


#define SCM_TICK scm_async_tick ()



#ifndef SCM_MAGIC_SNARFER
/* Let these macros pass through if
   we are snarfing;  thus we can tell the
   difference between the use of an actual
   number vs. the use of one of these macros --
   actual numbers in SCM_VALIDATE_* and SCM_ASSERT
   constructs must match the formal argument name,
   but using SCM_ARG* avoids the test */

#define SCM_ARGn 		0
#define SCM_ARG1 		1
#define SCM_ARG2 		2
#define SCM_ARG3 		3
#define SCM_ARG4 		4
#define SCM_ARG5 		5
#define SCM_ARG6 		6
#define SCM_ARG7 		7

#endif /* SCM_MAGIC_SNARFER */



/* Handling thread-local storage (TLS).  */

#ifdef SCM_HAVE_THREAD_STORAGE_CLASS
# define SCM_THREAD_LOCAL __thread
#else
# define SCM_THREAD_LOCAL
#endif

#endif  /* SCM___SCM_H */




/* Trivial type declarations and forward declarations.  */
typedef struct scm_print_state scm_print_state;
typedef struct scm_dynstack scm_t_dynstack;
typedef scm_t_int32 scm_t_wchar;



