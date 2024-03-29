/* examples/box-dynamic-module/box.c

   Copyright 1998,2001,2006
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

/* Include all needed declarations.  */
#include <libguile.h>


/* The type code for the newly created smob type will be stored into
   this variable.  It has the prefix `scm_tc16_' to make it usable
   with the SCM_VALIDATE_SMOB macro below.  */
static scm_t_bits scm_tc16_box;


/* This function is responsible for marking all SCM objects included
   in the smob.  */
static SCM
mark_box (SCM b)
{
  /* Since we have only one SCM object to protect, we simply return it
     and the caller will mark it.  */
  return SCM_CELL_OBJECT_1 (b);
}


/* Print a textual representation of the smob to a given port.  */
static int
print_box (SCM b, SCM port, scm_print_state *pstate)
{
  SCM value = SCM_CELL_OBJECT_1 (b);

  scm_puts ("#<box ", port);
  scm_write (value, port);
  scm_puts (">", port);

  /* Non-zero means success.  */
  return 1;
}


/* This defines the primitive `make-box', which returns a new smob of
   type `box', initialized to `#f'.  */
static SCM
#define FUNC_NAME "make-box"
make_box (void)
{
  /* This macro creates the new objects, stores the value `#f' into it
     and returns it to the caller.  */
  SCM_RETURN_NEWSMOB (scm_tc16_box, SCM_BOOL_F);
}
#undef FUNC_NAME


/* This is the primitive `box-ref' which returns the object stored in
   the box.  */
static SCM
box_ref (SCM b)
#define FUNC_NAME "box-ref"
{
  /* First, we have to ensure that the user really gave us a box
     objects.  The macro SCM_VALIDATE_SMOB will do all what is needed.
     The parameters are interpreted as follows: 

     1: The position of the checked variable in the parameter list.
     b: The passed parameter.
     box: Concatenated with the fixed prefix scm_tc16_, names the type
          code for the expected smob type.  */
  SCM_VALIDATE_SMOB (1, b, box);

  /* Fetch the object from the box and return it.  */
  return SCM_CELL_OBJECT_1 (b);
}
#undef FUNC_NAME


/* Primitive which stores an arbitrary value into a box.  */
static SCM
box_set_x (SCM b, SCM value)
#define FUNC_NAME "box-set!"
{
  SCM_VALIDATE_SMOB (1, b, box);

  /* Set the cell number 1 of the smob to the given value.  */
  SCM_SET_CELL_OBJECT_1 (b, value);

  /* When this constant is returned, the REPL will not print the
     returned value.  All procedures in Guile which are documented as
     returning `and unspecified value' actually return this value.  */
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME


/* This is the function which must be given to `load-extension' as the
   second argument.  In this example, the Scheme file box-module.scm
   (or box-mixed.scm) is responsible for doing the load-extension
   call.  The Scheme modules are also responsible for placing the
   procedure definitions in the correct module. */
void
scm_init_box ()
{
  scm_tc16_box = scm_make_smob_type ("box", 0);
  scm_set_smob_mark (scm_tc16_box, mark_box);
  scm_set_smob_print (scm_tc16_box, print_box);

  scm_c_define_gsubr ("make-box", 0, 0, 0, make_box);
  scm_c_define_gsubr ("box-set!", 2, 0, 0, box_set_x);
  scm_c_define_gsubr ("box-ref", 1, 0, 0, box_ref);
}

/* End of file.  */
