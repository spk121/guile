/* Copyright 2003-2004,2006,2008-2018,2020,2021,2022
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

#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define SCM_BUILDING_DEPRECATED_CODE

#include "alist.h"
#include "array-handle.h"
#include "arrays.h"
#include "boolean.h"
#include "bitvectors.h"
#include "deprecation.h"
#include "dynl.h"
#include "eval.h"
#include "foreign.h"
#include "finalizers.h"
#include "generalized-vectors.h"
#include "gc.h"
#include "gsubr.h"
#include "modules.h"
#include "objprop.h"
#include "procprop.h"
#include "srcprop.h"
#include "srfi-4.h"
#include "strings.h"
#include "symbols.h"
#include "uniform.h"
#include "vectors.h"

#include "deprecated.h"

#if (SCM_ENABLE_DEPRECATED == 1)



#ifndef MAXPATHLEN
#define MAXPATHLEN 80
#endif /* ndef MAXPATHLEN */
#ifndef X_OK
#define X_OK 1
#endif /* ndef X_OK */

char *
scm_find_executable (const char *name)
{
  char tbuf[MAXPATHLEN];
  int i = 0, c;
  FILE *f;

  scm_c_issue_deprecation_warning ("scm_find_executable is deprecated.");

  /* fprintf(stderr, "s_f_e checking access %s ->%d\n", name, access(name, X_OK)); fflush(stderr); */
  if (access (name, X_OK))
    return 0L;
  f = fopen (name, "r");
  if (!f)
    return 0L;
  if ((fgetc (f) == '#') && (fgetc (f) == '!'))
    {
      while (1)
	switch (c = fgetc (f))
	  {
	  case /*WHITE_SPACES */ ' ':
	  case '\t':
	  case '\r':
	  case '\f':
	  case EOF:
	    tbuf[i] = 0;
	    fclose (f);
	    return strdup (tbuf);
	  default:
	    tbuf[i++] = c;
	    break;
	  }
    }
  fclose (f);
  return strdup (name);
}




int
scm_is_simple_vector (SCM obj)
{
  scm_c_issue_deprecation_warning
    ("scm_is_simple_vector is deprecated. Use scm_is_vector instead.");
  return SCM_I_IS_VECTOR (obj);
}

SCM
scm_bitvector_p (SCM vec)
{
  scm_c_issue_deprecation_warning
    ("scm_bitvector_p is deprecated.  Use scm_is_bitvector instead.");

  return scm_from_bool (scm_is_bitvector (vec));
}

SCM
scm_bitvector (SCM list)
{
  scm_c_issue_deprecation_warning
    ("scm_bitvector is deprecated.  Use scm_list_to_bitvector instead.");

  return scm_list_to_bitvector (list);
}

SCM
scm_make_bitvector (SCM len, SCM fill)
{
  scm_c_issue_deprecation_warning
    ("scm_make_bitvector is deprecated.  Use scm_c_make_bitvector instead.");

  return scm_c_make_bitvector (scm_to_size_t (len), fill);
}

SCM
scm_bitvector_length (SCM vec)
{
  scm_c_issue_deprecation_warning
    ("scm_bitvector_length is deprecated.  Use scm_c_bitvector_length "
     "instead.");

  return scm_from_size_t (scm_c_bitvector_length (vec));
}

SCM
scm_c_bitvector_ref (SCM vec, size_t idx)
{
  scm_c_issue_deprecation_warning
    ("bitvector-ref is deprecated.  Use bitvector-bit-set? instead.");

  if (scm_is_bitvector (vec))
    return scm_from_bool (scm_c_bitvector_bit_is_set (vec, idx));

  SCM res;
  scm_t_array_handle handle;
  size_t len, off;
  ssize_t inc;

  const uint32_t *bits =
    scm_bitvector_elements (vec, &handle, &off, &len, &inc);

  if (idx >= len)
    scm_out_of_range (NULL, scm_from_size_t (idx));
  idx = idx*inc + off;
  res = scm_from_bool (bits[idx/32] & (1L << (idx%32)));
  scm_array_handle_release (&handle);
  return res;
}

SCM_DEFINE (scm_bitvector_ref, "bitvector-ref", 2, 0, 0,
	    (SCM vec, SCM idx),
	    "Return the element at index @var{idx} of the bitvector\n"
	    "@var{vec}.")
#define FUNC_NAME s_scm_bitvector_ref
{
  return scm_c_bitvector_ref (vec, scm_to_size_t (idx));
}
#undef FUNC_NAME

void
scm_c_bitvector_set_x (SCM vec, size_t idx, SCM val)
{
  scm_c_issue_deprecation_warning
    ("bitvector-set! is deprecated.  Use bitvector-set-bit! or "
     "bitvector-clear-bit! instead.");

  if (scm_is_bitvector (vec))
    {
      if (scm_is_true (val))
        scm_c_bitvector_set_bit_x (vec, idx);
      else
        scm_c_bitvector_clear_bit_x (vec, idx);
    }
  else
    {
      scm_t_array_handle handle;
      uint32_t *bits, mask;
      size_t len, off;
      ssize_t inc;
  
      bits = scm_bitvector_writable_elements (vec, &handle, &off, &len, &inc);
      if (idx >= len)
	scm_out_of_range (NULL, scm_from_size_t (idx));
      idx = idx*inc + off;

      mask = 1L << (idx%32);
      if (scm_is_true (val))
        bits[idx/32] |= mask;
      else
        bits[idx/32] &= ~mask;

      scm_array_handle_release (&handle);
    }
}

SCM_DEFINE (scm_bitvector_set_x, "bitvector-set!", 3, 0, 0,
	    (SCM vec, SCM idx, SCM val),
	    "Set the element at index @var{idx} of the bitvector\n"
	    "@var{vec} when @var{val} is true, else clear it.")
#define FUNC_NAME s_scm_bitvector_set_x
{
  scm_c_bitvector_set_x (vec, scm_to_size_t (idx), val);
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bitvector_fill_x, "bitvector-fill!", 2, 0, 0,
	    (SCM vec, SCM val),
	    "Set all elements of the bitvector\n"
	    "@var{vec} when @var{val} is true, else clear them.")
#define FUNC_NAME s_scm_bitvector_fill_x
{
  scm_c_issue_deprecation_warning
    ("bitvector-fill! is deprecated.  Use bitvector-set-all-bits! or "
     "bitvector-clear-all-bits! instead.");

  if (scm_is_bitvector (vec))
    {
      if (scm_is_true (val))
        scm_c_bitvector_set_all_bits_x (vec);
      else
        scm_c_bitvector_clear_all_bits_x (vec);

      return SCM_UNSPECIFIED;
    }

  scm_t_array_handle handle;
  size_t off, len;
  ssize_t inc;

  scm_bitvector_writable_elements (vec, &handle, &off, &len, &inc);

  size_t i;
  for (i = 0; i < len; i++)
    scm_array_handle_set (&handle, i*inc, val);

  scm_array_handle_release (&handle);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bit_invert_x, "bit-invert!", 1, 0, 0,
           (SCM v),
	    "Modify the bit vector @var{v} by replacing each element with\n"
	    "its negation.")
#define FUNC_NAME s_scm_bit_invert_x
{
  scm_c_issue_deprecation_warning
    ("bit-invert! is deprecated.  Use bitvector-flip-all-bits!, or  "
     "scalar array accessors in a loop for generic arrays.");

  if (scm_is_bitvector (v))
    scm_c_bitvector_flip_all_bits_x (v);
  else
    {
      size_t off, len;
      ssize_t inc;
      scm_t_array_handle handle;

      scm_bitvector_writable_elements (v, &handle, &off, &len, &inc);
      for (size_t i = 0; i < len; i++)
	scm_array_handle_set (&handle, i*inc,
			      scm_not (scm_array_handle_ref (&handle, i*inc)));
      scm_array_handle_release (&handle);
    }

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bit_count, "bit-count", 2, 0, 0,
	    (SCM b, SCM bitvector),
	    "Return the number of occurrences of the boolean @var{b} in\n"
	    "@var{bitvector}.")
#define FUNC_NAME s_scm_bit_count
{
  int bit = scm_to_bool (b);
  size_t count = 0, len;

  scm_c_issue_deprecation_warning
    ("bit-count is deprecated.  Use bitvector-count, or a loop over array-ref "
     "if array support is needed.");

  if (scm_is_bitvector (bitvector))
    {
      len = scm_to_size_t (scm_bitvector_length (bitvector));
      count = scm_c_bitvector_count (bitvector);
    }
  else
    {
      scm_t_array_handle handle;
      size_t off;
      ssize_t inc;

      scm_bitvector_elements (bitvector, &handle, &off, &len, &inc);

      for (size_t i = 0; i < len; i++)
	if (scm_is_true (scm_array_handle_ref (&handle, i*inc)))
	  count++;

      scm_array_handle_release (&handle);
    }
  
  return scm_from_size_t (bit ? count : len-count);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bit_count_star, "bit-count*", 3, 0, 0,
           (SCM v, SCM kv, SCM obj),
	    "Return a count of how many entries in bit vector @var{v} are\n"
	    "equal to @var{obj}, with @var{kv} selecting the entries to\n"
	    "consider.\n"
	    "\n"
	    "If @var{kv} is a bit vector, then those entries where it has\n"
	    "@code{#t} are the ones in @var{v} which are considered.\n"
	    "@var{kv} and @var{v} must be the same length.\n"
	    "\n"
	    "If @var{kv} is a u32vector, then it contains\n"
	    "the indexes in @var{v} to consider.\n"
	    "\n"
	    "For example,\n"
	    "\n"
	    "@example\n"
	    "(bit-count* #*01110111 #*11001101 #t) @result{} 3\n"
	    "(bit-count* #*01110111 #u32(7 0 4) #f)  @result{} 2\n"
	    "@end example")
#define FUNC_NAME s_scm_bit_count_star
{
  size_t count = 0;

  scm_c_issue_deprecation_warning
    ("bit-count* is deprecated.  Use bitvector-count-bits instead, and in the "
     "case of counting false bits, subtract from a bitvector-count on the "
     "selection bitvector.");

  /* Validate that OBJ is a boolean so this is done even if we don't
     need BIT.
  */
  int bit = scm_to_bool (obj);

  if (scm_is_bitvector (v) && scm_is_bitvector (kv))
    {
      count = scm_c_bitvector_count_bits (v, kv);
      if (bit == 0)
        count = scm_c_bitvector_count (kv) - count;
    }
  else
    {
      scm_t_array_handle v_handle;
      size_t v_off, v_len;
      ssize_t v_inc;

      scm_bitvector_elements (v, &v_handle, &v_off, &v_len, &v_inc);

      if (scm_is_bitvector (kv))
        {
          size_t kv_len = scm_c_bitvector_length (kv);
          for (size_t i = 0; i < kv_len; i++)
            if (scm_c_bitvector_bit_is_set (kv, i))
              {
                SCM elt = scm_array_handle_ref (&v_handle, i*v_inc);
                if ((bit && scm_is_true (elt)) || (!bit && scm_is_false (elt)))
                  count++;
              }
        }
      else if (scm_is_true (scm_u32vector_p (kv)))
        {
          scm_t_array_handle kv_handle;
          size_t i, kv_len;
          ssize_t kv_inc;
          const uint32_t *kv_elts;

          kv_elts = scm_u32vector_elements (kv, &kv_handle, &kv_len, &kv_inc);

          for (i = 0; i < kv_len; i++, kv_elts += kv_inc)
            {
              SCM elt = scm_array_handle_ref (&v_handle, (*kv_elts)*v_inc);
              if ((bit && scm_is_true (elt)) || (!bit && scm_is_false (elt)))
                count++;
            }

          scm_array_handle_release (&kv_handle);
        }
      else
        scm_wrong_type_arg_msg (NULL, 0, kv, "bitvector or u32vector");

      scm_array_handle_release (&v_handle);
    }

  return scm_from_size_t (count);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bit_position, "bit-position", 3, 0, 0,
           (SCM item, SCM v, SCM k),
	    "Return the index of the first occurrence of @var{item} in bit\n"
	    "vector @var{v}, starting from @var{k}.  If there is no\n"
	    "@var{item} entry between @var{k} and the end of\n"
	    "@var{v}, then return @code{#f}.  For example,\n"
	    "\n"
	    "@example\n"
	    "(bit-position #t #*000101 0)  @result{} 3\n"
	    "(bit-position #f #*0001111 3) @result{} #f\n"
	    "@end example")
#define FUNC_NAME s_scm_bit_position
{
  scm_c_issue_deprecation_warning
    ("bit-position is deprecated.  Use bitvector-position, or "
     "array-ref in a loop if you need generic arrays instead.");

  if (scm_is_bitvector (v))
    return scm_bitvector_position (v, item, k);

  scm_t_array_handle handle;
  size_t off, len;
  ssize_t inc;
  scm_bitvector_elements (v, &handle, &off, &len, &inc);
  int bit = scm_to_bool (item);
  size_t first_bit = scm_to_unsigned_integer (k, 0, len);
  SCM res = SCM_BOOL_F;
  for (size_t i = first_bit; i < len; i++)
    {
      SCM elt = scm_array_handle_ref (&handle, i*inc);
      if ((bit && scm_is_true (elt)) || (!bit && scm_is_false (elt)))
        {
          res = scm_from_size_t (i);
          break;
        }
    }
  scm_array_handle_release (&handle);

  return res;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bit_set_star_x, "bit-set*!", 3, 0, 0,
	    (SCM v, SCM kv, SCM obj),
	    "Set entries of bit vector @var{v} to @var{obj}, with @var{kv}\n"
	    "selecting the entries to change.  The return value is\n"
	    "unspecified.\n"
	    "\n"
	    "If @var{kv} is a bit vector, then those entries where it has\n"
	    "@code{#t} are the ones in @var{v} which are set to @var{obj}.\n"
	    "@var{v} must be at least as long as @var{kv}.  When @var{obj}\n"
	    "is @code{#t} it's like @var{kv} is OR'ed into @var{v}.  Or when\n"
	    "@var{obj} is @code{#f} it can be seen as an ANDNOT.\n"
	    "\n"
	    "@example\n"
	    "(define bv #*01000010)\n"
	    "(bit-set*! bv #*10010001 #t)\n"
	    "bv\n"
	    "@result{} #*11010011\n"
	    "@end example\n"
	    "\n"
	    "If @var{kv} is a u32vector, then its elements are\n"
	    "indices into @var{v} which are set to @var{obj}.\n"
	    "\n"
	    "@example\n"
	    "(define bv #*01000010)\n"
	    "(bit-set*! bv #u32(5 2 7) #t)\n"
	    "bv\n"
	    "@result{} #*01100111\n"
	    "@end example")
#define FUNC_NAME s_scm_bit_set_star_x
{
  scm_c_issue_deprecation_warning
    ("bit-set*! is deprecated.  Use bitvector-set-bits! or "
     "bitvector-clear-bits! on bitvectors, or array-set! in a loop "
     "if you need to work on generic arrays.");

  int bit = scm_to_bool (obj);
  if (scm_is_bitvector (v) && scm_is_bitvector (kv))
    {
      if (bit)
        scm_c_bitvector_set_bits_x (v, kv);
      else
        scm_c_bitvector_clear_bits_x (v, kv);

      return SCM_UNSPECIFIED;
    }

  scm_t_array_handle v_handle;
  size_t v_off, v_len;
  ssize_t v_inc;
  scm_bitvector_writable_elements (v, &v_handle, &v_off, &v_len, &v_inc);

  if (scm_is_bitvector (kv))
    {
      size_t kv_len = scm_c_bitvector_length (kv);

      if (v_len < kv_len)
        scm_misc_error (NULL,
                        "selection bitvector longer than target bitvector",
                        SCM_EOL);

      for (size_t i = 0; i < kv_len; i++)
        if (scm_is_true (scm_c_bitvector_ref (kv, i)))
          scm_array_handle_set (&v_handle, i*v_inc, obj);
    }
  else if (scm_is_true (scm_u32vector_p (kv)))
    {
      scm_t_array_handle kv_handle;
      size_t kv_len;
      ssize_t kv_inc;
      const uint32_t *kv_elts;

      kv_elts = scm_u32vector_elements (kv, &kv_handle, &kv_len, &kv_inc);
      for (size_t i = 0; i < kv_len; i++, kv_elts += kv_inc)
        scm_array_handle_set (&v_handle, (*kv_elts)*v_inc, obj);

      scm_array_handle_release (&kv_handle);
    }
  else
    scm_wrong_type_arg_msg (NULL, 0, kv, "bitvector or u32vector");

  scm_array_handle_release (&v_handle);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_istr2bve (SCM str)
{
  scm_t_array_handle handle;
  size_t len = scm_i_string_length (str);
  SCM vec = scm_c_make_bitvector (len, SCM_UNDEFINED);
  SCM res = vec;

  uint32_t mask;
  size_t k, j;
  const char *c_str;
  uint32_t *data;

  scm_c_issue_deprecation_warning
    ("scm_istr2bve is deprecated.  "
     "Read from a string instead, prefixed with `#*'.");

  data = scm_bitvector_writable_elements (vec, &handle, NULL, NULL, NULL);
  c_str = scm_i_string_chars (str);

  for (k = 0; k < (len + 31) / 32; k++)
    {
      data[k] = 0L;
      j = len - k * 32;
      if (j > 32)
	j = 32;
      for (mask = 1L; j--; mask <<= 1)
	switch (*c_str++)
	  {
	  case '0':
	    break;
	  case '1':
	    data[k] |= mask;
	    break;
	  default:
	    res = SCM_BOOL_F;
	    goto exit;
	  }
    }
  
 exit:
  scm_array_handle_release (&handle);
  scm_remember_upto_here_1 (str);
  return res;
}

SCM
scm_from_contiguous_typed_array (SCM type, SCM bounds, const void *bytes,
                                 size_t byte_len)
#define FUNC_NAME "scm_from_contiguous_typed_array"
{
  size_t k, rlen = 1;
  scm_t_array_dim *s;
  SCM ra;
  scm_t_array_handle h;
  void *elts;
  size_t sz;

  scm_c_issue_deprecation_warning
    ("scm_from_contiguous_typed_array is deprecated.  "
     "Instead, use scm_make_typed_array() and the array handle functions "
     "to copy data to the new array.");

  ra = scm_i_shap2ra (bounds);
  s = SCM_I_ARRAY_DIMS (ra);
  k = SCM_I_ARRAY_NDIM (ra);

  while (k--)
    {
      s[k].inc = rlen;
      SCM_ASSERT_RANGE (1, bounds, s[k].lbnd <= s[k].ubnd + 1);
      rlen = (s[k].ubnd - s[k].lbnd + 1) * s[k].inc;
    }
  SCM_I_ARRAY_SET_V (ra, scm_make_generalized_vector (type, scm_from_size_t (rlen), SCM_UNDEFINED));


  scm_array_get_handle (ra, &h);
  elts = h.writable_elements;
  sz = scm_array_handle_uniform_element_bit_size (&h);
  scm_array_handle_release (&h);

  if (sz >= 8 && ((sz % 8) == 0))
    {
      if (byte_len % (sz / 8))
        SCM_MISC_ERROR ("byte length not a multiple of the unit size", SCM_EOL);
      if (byte_len / (sz / 8) != rlen)
        SCM_MISC_ERROR ("byte length and dimensions do not match", SCM_EOL);
    }
  else if (sz < 8)
    {
      /* Elements of sub-byte size (bitvectors) are addressed in 32-bit
         units.  */
      if (byte_len != ((rlen * sz + 31) / 32) * 4)
        SCM_MISC_ERROR ("byte length and dimensions do not match", SCM_EOL);
    }
  else
    /* an internal guile error, really */
    SCM_MISC_ERROR ("uniform elements larger than 8 bits must fill whole bytes", SCM_EOL);

  memcpy (elts, bytes, byte_len);

  if (1 == SCM_I_ARRAY_NDIM (ra) && 0 == SCM_I_ARRAY_BASE (ra))
    if (0 == s->lbnd)
      return SCM_I_ARRAY_V (ra);
  return ra;
}
#undef FUNC_NAME


SCM_GLOBAL_SYMBOL (scm_sym_copy, "copy");

SCM
scm_make_srcprops (long line, int col, SCM filename, SCM copy, SCM alist)
{
  scm_c_issue_deprecation_warning
    ("scm_make_srcprops is deprecated; use set-source-properties! instead");

  alist = SCM_UNBNDP (copy) ? alist : scm_acons (scm_sym_copy, copy, alist);
  return scm_i_make_srcprops (scm_from_long (line), scm_from_int (col),
                              filename, alist);
}

SCM
scm_copy_tree (SCM obj)
{
  scm_c_issue_deprecation_warning
    ("scm_copy_tree is deprecated; use copy-tree from (ice-9 copy-tree) "
     "instead.");

  return scm_call_1 (scm_c_public_ref ("ice-9 copy-tree", "copy-tree"), obj);
}


/* Symbol properties.  */

SCM_SYMBOL (symbol_function_slot, "symbol-function-slot");
SCM_SYMBOL (symbol_property_slot, "symbol-property-slot");

SCM_DEFINE (scm_symbol_fref, "symbol-fref", 1, 0, 0, 
           (SCM s),
	    "Return the contents of the symbol @var{s}'s @dfn{function slot}.")
#define FUNC_NAME s_scm_symbol_fref
{
  SCM_VALIDATE_SYMBOL (1, s);
  return scm_object_property (s, symbol_function_slot);
}
#undef FUNC_NAME

SCM_DEFINE (scm_symbol_pref, "symbol-pref", 1, 0, 0, 
           (SCM s),
	    "Return the @dfn{property list} currently associated with the\n"
	    "symbol @var{s}.")
#define FUNC_NAME s_scm_symbol_pref
{
  SCM result;

  SCM_VALIDATE_SYMBOL (1, s);
  result = scm_object_property (s, symbol_property_slot);
  return scm_is_false (result) ? SCM_EOL : result;
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_fset_x, "symbol-fset!", 2, 0, 0, 
           (SCM s, SCM val),
	    "Change the binding of the symbol @var{s}'s function slot.")
#define FUNC_NAME s_scm_symbol_fset_x
{
  SCM_VALIDATE_SYMBOL (1, s);
  return scm_set_object_property_x (s, symbol_function_slot, val);
}
#undef FUNC_NAME


SCM_DEFINE (scm_symbol_pset_x, "symbol-pset!", 2, 0, 0,
           (SCM s, SCM val),
	    "Change the binding of the symbol @var{s}'s property slot.")
#define FUNC_NAME s_scm_symbol_pset_x
{
  SCM_VALIDATE_SYMBOL (1, s);
  return scm_set_object_property_x (s, symbol_property_slot, val);
}
#undef FUNC_NAME



SCM_DEFINE (scm_dynamic_unlink, "dynamic-unlink", 1, 0, 0, (SCM obj), "")
#define FUNC_NAME s_scm_dynamic_unlink
{
  scm_c_issue_deprecation_warning
    ("scm_dynamic_unlink has no effect and is deprecated.  Unloading "
     "shared libraries is no longer supported.");
  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME




static void
finalize_bignum (void *ptr, void *data)
{
  SCM bignum;

  bignum = SCM_PACK_POINTER (ptr);
  mpz_clear (SCM_I_BIG_MPZ (bignum));
}

static SCM
make_bignum (void)
{
  scm_t_bits *p;

  /* Allocate one word for the type tag and enough room for an `mpz_t'.  */
  p = scm_gc_malloc_pointerless (sizeof (scm_t_bits) + sizeof (mpz_t),
                                 "bignum");
  p[0] = scm_tc16_big;
  scm_i_set_finalizer (p, finalize_bignum, NULL);

  return SCM_PACK (p);
}

/* scm_i_big2dbl() rounds to the closest representable double,
   in accordance with R5RS exact->inexact.  */
double
scm_i_big2dbl (SCM b)
{
  scm_c_issue_deprecation_warning
    ("scm_i_big2dbl is deprecated.  Use scm_to_double instead.");
  return scm_to_double (b);
}

SCM
scm_i_long2big (long x)
{
  scm_c_issue_deprecation_warning
    ("scm_i_long2big is deprecated.  Use scm_from_long instead.");
  /* Return a newly created bignum initialized to X. */
  SCM z = make_bignum ();
  mpz_init_set_si (SCM_I_BIG_MPZ (z), x);
  return z;
}

SCM
scm_i_ulong2big (unsigned long x)
{
  scm_c_issue_deprecation_warning
    ("scm_i_ulong2big is deprecated.  Use scm_from_ulong instead.");
  /* Return a newly created bignum initialized to X. */
  SCM z = make_bignum ();
  mpz_init_set_ui (SCM_I_BIG_MPZ (z), x);
  return z;
}

SCM
scm_i_clonebig (SCM src_big, int same_sign_p)
{
  scm_c_issue_deprecation_warning
    ("scm_i_clonebig is deprecated.  Use scm_to_mpz/scm_from_mpz instead.");
  /* Copy src_big's value, negate it if same_sign_p is false, and return. */
  SCM z = make_bignum ();
  scm_to_mpz (src_big, SCM_I_BIG_MPZ (z));
  if (!same_sign_p)
    mpz_neg (SCM_I_BIG_MPZ (z), SCM_I_BIG_MPZ (z));
  return z;
}

SCM
scm_i_normbig (SCM b)
{
  scm_c_issue_deprecation_warning
    ("scm_i_normbig is deprecated.  Direct bignum bit manipulation is not "
     "supported.");
  /* convert a big back to a fixnum if it'll fit */
  /* presume b is a bignum */
  if (mpz_fits_slong_p (SCM_I_BIG_MPZ (b)))
    {
      intptr_t val = mpz_get_si (SCM_I_BIG_MPZ (b));
      if (SCM_FIXABLE (val))
        b = SCM_I_MAKINUM (val);
    }
  return b;
}

int scm_install_gmp_memory_functions;



void
scm_i_init_deprecated ()
{
#include "deprecated.x"
}

#endif /* SCM_ENABLE_DEPRECATD == 1 */
