/* Copyright 1995-1996,1998,2000-2001,2003-2004,2006,2008-2013,2018
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




#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "alist.h"
#include "boolean.h"
#include "eval.h"
#include "gsubr.h"
#include "list.h"
#include "numbers.h"
#include "pairs.h"
#include "procs.h"
#include "programs.h"
#include "smob.h"
#include "symbols.h"
#include "threads.h"
#include "vectors.h"
#include "vm-builtins.h"
#include "weak-table.h"

#include "procprop.h"




SCM_GLOBAL_SYMBOL (scm_sym_system_procedure, "system-procedure");
SCM_GLOBAL_SYMBOL (scm_sym_name, "name");

static SCM overrides;

static SCM arity_overrides;

int
scm_i_procedure_arity (SCM proc, int *req, int *opt, int *rest)
{
  SCM o;

  o = scm_weak_table_refq (arity_overrides, proc, SCM_BOOL_F);

  if (scm_is_true (o))
    {
      *req = scm_to_int (scm_car (o));
      *opt = scm_to_int (scm_cadr (o));
      *rest = scm_is_true (scm_caddr (o));
      return 1;
    }

  while (!SCM_PROGRAM_P (proc))
    {
      if (SCM_STRUCTP (proc))
        {
          if (!SCM_STRUCT_APPLICABLE_P (proc))
            return 0;
          proc = SCM_STRUCT_PROCEDURE (proc);
        }
      else if (SCM_HAS_TYP7 (proc, scm_tc7_smob))
        {
          if (!SCM_SMOB_APPLICABLE_P (proc))
            return 0;
          if (!scm_i_program_arity (SCM_SMOB_DESCRIPTOR (proc).apply_trampoline,
                                    req, opt, rest))
            return 0;

          /* The trampoline gets the smob too, which users don't
             see.  */
          *req -= 1;

          return 1;
        }
      else
        return 0;
    }

  return scm_i_program_arity (proc, req, opt, rest);
}

SCM_DEFINE (scm_set_procedure_minimum_arity_x, "set-procedure-minimum-arity!",
            4, 0, 0, (SCM proc, SCM req, SCM opt, SCM rest),
            "")
#define FUNC_NAME s_scm_set_procedure_minimum_arity_x
{
  int t SCM_UNUSED;

  SCM_VALIDATE_PROC (1, proc);
  SCM_VALIDATE_INT_COPY (2, req, t);
  SCM_VALIDATE_INT_COPY (3, opt, t);
  SCM_VALIDATE_BOOL (4, rest);

  scm_weak_table_putq_x (arity_overrides, proc, scm_list_3 (req, opt, rest));
  return SCM_UNDEFINED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_minimum_arity, "procedure-minimum-arity", 1, 0, 0, 
           (SCM proc),
	    "Return the \"minimum arity\" of a procedure.\n\n"
            "If the procedure has only one arity, that arity is returned\n"
            "as a list of three values: the number of required arguments,\n"
            "the number of optional arguments, and a boolean indicating\n"
            "whether or not the procedure takes rest arguments.\n\n"
            "For a case-lambda procedure, the arity returned is the one\n"
            "with the lowest minimum number of arguments, and the highest\n"
            "maximum number of arguments.\n\n"
            "If it was not possible to determine the arity of the procedure,\n"
            "@code{#f} is returned.")
#define FUNC_NAME s_scm_procedure_minimum_arity
{
  int req, opt, rest;
  
  if (scm_i_procedure_arity (proc, &req, &opt, &rest))
    return scm_list_3 (scm_from_int (req),
                       scm_from_int (opt),
                       scm_from_bool (rest));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_properties, "procedure-properties", 1, 0, 0, 
           (SCM proc),
	    "Return @var{proc}'s property list.")
#define FUNC_NAME s_scm_procedure_properties
{
  SCM ret, user_props;
  
  SCM_VALIDATE_PROC (1, proc);

  user_props = scm_weak_table_refq (overrides, proc, SCM_BOOL_F);

  if (scm_is_pair (user_props) && scm_is_true (scm_car (user_props)))
    return scm_cdr (user_props);

  if (SCM_PROGRAM_P (proc))
    ret = scm_i_program_properties (proc);
  else
    ret = SCM_EOL;

  if (scm_is_pair (user_props))
    for (user_props = scm_cdr (user_props);
         scm_is_pair (user_props);
         user_props = scm_cdr (user_props))
      ret = scm_assq_set_x (ret, scm_caar (user_props), scm_cdar (user_props));

  return ret;
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_properties_x, "set-procedure-properties!", 2, 0, 0,
           (SCM proc, SCM alist),
	    "Set @var{proc}'s property list to @var{alist}.")
#define FUNC_NAME s_scm_set_procedure_properties_x
{
  SCM_VALIDATE_PROC (1, proc);

  scm_weak_table_putq_x (overrides, proc, scm_cons (SCM_BOOL_T, alist));

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_procedure_property, "procedure-property", 2, 0, 0,
           (SCM proc, SCM key),
	    "Return the property of @var{proc} with name @var{key}.")
#define FUNC_NAME s_scm_procedure_property
{
  SCM user_props;

  SCM_VALIDATE_PROC (1, proc);

  if (scm_is_eq (key, scm_sym_name))
    return scm_procedure_name (proc);
  if (scm_is_eq (key, scm_sym_documentation))
    return scm_procedure_documentation (proc);

  user_props = scm_weak_table_refq (overrides, proc, SCM_BOOL_F);
  if (scm_is_true (user_props)) 
    {
      SCM pair = scm_assq (key, scm_cdr (user_props));
      if (scm_is_pair (pair))
        return scm_cdr (pair);
      if (scm_is_true (scm_car (user_props)))
        return SCM_BOOL_F;
    }

  return scm_assq_ref (scm_procedure_properties (proc), key);
}
#undef FUNC_NAME

SCM_DEFINE (scm_set_procedure_property_x, "set-procedure-property!", 3, 0, 0,
           (SCM proc, SCM key, SCM val),
	    "In @var{proc}'s property list, set the property named @var{key} to\n"
	    "@var{val}.")
#define FUNC_NAME s_scm_set_procedure_property_x
{
  SCM user_props, override_p;

  SCM_VALIDATE_PROC (1, proc);

  scm_i_pthread_mutex_lock (&scm_i_misc_mutex);
  user_props = scm_weak_table_refq (overrides, proc, SCM_BOOL_F);
  if (scm_is_false (user_props))
    {
      override_p = SCM_BOOL_F;
      user_props = SCM_EOL;
    }
  else
    {
      override_p = scm_car (user_props);
      user_props = scm_cdr (user_props);
    }
  scm_weak_table_putq_x (overrides, proc,
                         scm_cons (override_p,
                                   scm_assq_set_x (user_props, key, val)));
  scm_i_pthread_mutex_unlock (&scm_i_misc_mutex);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




SCM_SYMBOL (scm_sym_source, "source");


SCM_DEFINE (scm_procedure_name, "procedure-name", 1, 0, 0,
            (SCM proc),
	    "Return the name of the procedure @var{proc}")
#define FUNC_NAME s_scm_procedure_name
{
  SCM user_props;

  SCM_VALIDATE_PROC (1, proc);

  user_props = scm_weak_table_refq (overrides, proc, SCM_BOOL_F);
  if (scm_is_true (user_props)) 
    {
      SCM pair = scm_assq (scm_sym_name, scm_cdr (user_props));
      if (scm_is_pair (pair))
        return scm_cdr (pair);
      if (scm_is_true (scm_car (user_props)))
        return SCM_BOOL_F;
    }

  if (SCM_PROGRAM_P (proc))
    return scm_i_program_name (proc);
  else if (SCM_STRUCTP (proc) && SCM_STRUCT_APPLICABLE_P (proc))
    return scm_procedure_name (SCM_STRUCT_PROCEDURE (proc));
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_GLOBAL_SYMBOL (scm_sym_documentation, "documentation");

SCM_DEFINE (scm_procedure_documentation, "procedure-documentation", 1, 0, 0,
           (SCM proc),
	    "Return the documentation string associated with @code{proc}.  By\n"
	    "convention, if a procedure contains more than one expression and the\n"
	    "first expression is a string constant, that string is assumed to contain\n"
	    "documentation for that procedure.")
#define FUNC_NAME s_scm_procedure_documentation
{
  SCM user_props;

  SCM_VALIDATE_PROC (1, proc);

  while (SCM_STRUCTP (proc) && SCM_STRUCT_APPLICABLE_P (proc))
    proc = SCM_STRUCT_PROCEDURE (proc);

  user_props = scm_weak_table_refq (overrides, proc, SCM_BOOL_F);
  if (scm_is_true (user_props)) 
    {
      SCM pair = scm_assq (scm_sym_documentation, scm_cdr (user_props));
      if (scm_is_pair (pair))
        return scm_cdr (pair);
      if (scm_is_true (scm_car (user_props)))
        return SCM_BOOL_F;
    }

  if (SCM_PROGRAM_P (proc))
    return scm_i_program_documentation (proc);
  else
    return SCM_BOOL_F;
}
#undef FUNC_NAME


SCM_DEFINE (scm_procedure_source, "procedure-source", 1, 0, 0,
            (SCM proc),
	    "Return the source of the procedure @var{proc}.")
#define FUNC_NAME s_scm_procedure_source
{
  SCM src;
  SCM_VALIDATE_PROC (1, proc);

  do
    {
      src = scm_procedure_property (proc, scm_sym_source);
      if (scm_is_true (src))
        return src;

      if (SCM_STRUCTP (proc) && SCM_STRUCT_APPLICABLE_P (proc)
          && SCM_HEAP_OBJECT_P ((proc = SCM_STRUCT_PROCEDURE (proc))))
        continue;
    }
  while (0);

  return SCM_BOOL_F;
}
#undef FUNC_NAME




void
scm_init_procprop ()
{
  overrides = scm_c_make_weak_table (0, SCM_WEAK_TABLE_KIND_KEY);
  arity_overrides = scm_c_make_weak_table (0, SCM_WEAK_TABLE_KIND_KEY);
#include "procprop.x"
  scm_init_vm_builtin_properties ();
}

