#include "test.h"

static void
run_test(jit_state_t *j, uint8_t *arena_base, size_t arena_size)
{
  jit_begin(j, arena_base, arena_size);
  size_t align = jit_enter_jit_abi(j, 0, 0, 0);
  jit_load_args_1(j, jit_operand_gpr (JIT_OPERAND_ABI_WORD, JIT_R0));

  jit_reloc_t r = jit_jmp(j);
  jit_patch_here(j, r);
  jit_leave_jit_abi(j, 0, 0, align);
  jit_retr(j, JIT_R0);

  jit_word_t (*f)(jit_word_t) = jit_end(j, NULL);
  ASSERT(f(42) == 42);
  ASSERT(f(-1) == -1);
}

int
main (int argc, char *argv[])
{
  return main_helper(argc, argv, run_test);
}
