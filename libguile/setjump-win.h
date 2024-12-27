#ifndef _SCM_SETJUMP_WIN_H_
#define _SCM_SETJUMP_WIN_H_

#include <setjmp.h>

/* On Windows, `setjmp` expands to _setjmp, which takes a second
   parameter that is set to the current frame address by default.
   The address is then stored as first element in the jump buffer.
   When `longjmp` is called, it tries to unwind the stack up
   to the saved address, which will fail, if the stack frames are
   interwoven with JIT-generated code.
   Set the second parameter to NULL to prevent unwinding. */
#undef setjmp
#define setjmp(env) _setjmp(env, NULL)

#endif /* _SCM_SETJUMP_WIN_H_ */

