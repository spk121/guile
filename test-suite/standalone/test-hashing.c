/* Copyright 2022-2023
     Free Software Foundation, Inc.

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

#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <libguile.h>

#include <stdio.h>

static void
test_hashing ()
{
  // Make sure a utf-8 symbol has the expected hash.  In addition to
  // catching algorithmic regressions, this would have caught a
  // long-standing buffer overflow.

  // Περί
  char about_u8[] = {0xce, 0xa0, 0xce, 0xb5, 0xcf, 0x81, 0xce, 0xaf, 0};
  SCM sym = scm_from_utf8_symbol (about_u8);

  // Value determined by calling wide_string_hash on {0x3A0, 0x3B5,
  // 0x3C1, 0x3AF} via a temporary test program.
  const unsigned long expect = 4029223418961680680;
  const unsigned long actual = scm_to_ulong (scm_symbol_hash (sym));

  if (actual != expect)
    {
      fprintf (stderr, "fail: unexpected utf-8 symbol hash (%lu != %lu)\n",
               actual, expect);
      exit (EXIT_FAILURE);
    }
}

static void
tests (void *data, int argc, char **argv)
{
  test_hashing ();
}

int
main (int argc, char *argv[])
{
  scm_boot_guile (argc, argv, tests, NULL);
  return 0;
}
