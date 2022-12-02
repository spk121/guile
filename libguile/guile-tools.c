#ifdef HAVE_CONFIG_H
#  include <config.h>
#endif

#include <locale.h>
#include <stdio.h>

#include <libguile.h>

static void
inner_main (void *closure SCM_UNUSED, int argc, char **argv)
{
  SCM mainproc;

  scm_c_use_module ("guild");
  mainproc = scm_c_private_ref("guild", "main");
  scm_call_1 (mainproc, scm_program_arguments());
}

int main(int argc, char **argv)
{
  setlocale (LC_ALL, "");
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0;
}
