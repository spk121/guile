/* Copyright 2009-2015,2018-2019,2023
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

#include <limits.h>
#include <byteswap.h>
#include <intprops.h>
#include <errno.h>
#include <striconveh.h>
#include <uniconv.h>
#include <unistr.h>
#include <string.h>
#include <alloca.h>
#include <assert.h>
#include <stdint.h>

#include "scm.h"

#if SCM_ENABLE_MINI_GMP
#include "mini-gmp.h"
#else
#include <gmp.h>
#endif

#include "array-handle.h"
#include "arrays.h"
#include "boolean.h"
#include "dynwind.h"
#include "extensions.h"
#include "generalized-vectors.h"
#include "gsubr.h"
#include "list.h"
#include "numbers.h"
#include "pairs.h"
#include "ports.h"
#include "srfi-4.h"
#include "strings.h"
#include "symbols.h"
#include "uniform.h"
#include "version.h"

#include "bytevectors.h"



/* Utilities.  */

/* Convenience macros.  These are used by the various templates (macros) that
   are parameterized by integer signedness.  */
#define INT8_T_signed           int8_t
#define INT8_T_unsigned         uint8_t
#define INT16_T_signed          int16_t
#define INT16_T_unsigned        uint16_t
#define INT32_T_signed          int32_t
#define INT32_T_unsigned        uint32_t
#if SIZEOF_INTPTR_T == 4
#define is_signed_int8(_x)      (((_x) >= INT32_C(-128)) && ((_x) <= INT32_C(127)))
#define is_unsigned_int8(_x)    ((_x) <= UINT32_C(255))
#define is_signed_int16(_x)     (((_x) >= INT32_C(-32768)) && ((_x) <= INT32_C(32767)))
#define is_unsigned_int16(_x)   ((_x) <= UINT32_C(65535))
#define is_signed_int32(_x)     (((_x) >= INT32_C(-2147483328)) && ((_x) <= INT32_C(2147483647)))
#define is_unsigned_int32(_x)   ((_x) <= UINT32_C(4294967295))
#elif SIZEOF_INTPTR_T == 8
#define is_signed_int8(_x)      (((_x) >= INT64_C(-128)) && ((_x) <= INT64_C(127)))
#define is_unsigned_int8(_x)    ((_x) <= UINT64_C(255))
#define is_signed_int16(_x)     (((_x) >= INT64_C(-32768)) && ((_x) <= INT64_C(32767)))
#define is_unsigned_int16(_x)   ((_x) <= UINT64_C(65535))
#define is_signed_int32(_x)     (((_x) >= INT64_C(-2147483648)) && ((_x) <= INT64_C(2147483647)))
#define is_unsigned_int32(_x)   ((_x) <= UINT64_C(4294967295))
#else
#error "Bad SIZEOF_INTPTR_T"
#endif
#define SIGNEDNESS_signed       1
#define SIGNEDNESS_unsigned     0

#define INT_TYPE(_size, _sign)  INT ## _size ## _T_ ## _sign
#define INT_SWAP(_size)         bswap_ ## _size
#define INT_VALID_P(_size, _sign) is_ ## _sign ## _int ## _size
#define SIGNEDNESS(_sign)       SIGNEDNESS_ ## _sign


#define INTEGER_ACCESSOR_PROLOGUE(validate, _len, _sign)        \
  size_t c_len, c_index;					\
  _sign char *c_bv;						\
								\
  SCM_VALIDATE_##validate (1, bv);                              \
  c_index = scm_to_size_t (index);				\
								\
  c_len = SCM_BYTEVECTOR_LENGTH (bv);				\
  c_bv = (_sign char *) SCM_BYTEVECTOR_CONTENTS (bv);		\
								\
  if (SCM_UNLIKELY (c_len < c_index				\
                    || (c_len - c_index < (_len) / 8)))		\
    scm_out_of_range (FUNC_NAME, index);

#define INTEGER_GETTER_PROLOGUE(_len, _sign)            \
  INTEGER_ACCESSOR_PROLOGUE (BYTEVECTOR, _len, _sign)

#define INTEGER_SETTER_PROLOGUE(_len, _sign)                    \
  INTEGER_ACCESSOR_PROLOGUE (MUTABLE_BYTEVECTOR, _len, _sign)

/* Template for fixed-size integer access (only 8, 16 or 32-bit).  */
#define INTEGER_REF(_len, _sign)                                \
  SCM result;                                                   \
                                                                \
  INTEGER_GETTER_PROLOGUE (_len, _sign);                        \
  SCM_VALIDATE_SYMBOL (3, endianness);                          \
                                                                \
  {                                                             \
      INT_TYPE (_len, _sign)  c_result;                         \
                                                                \
    memcpy (&c_result, &c_bv[c_index], (_len) / 8);             \
    if (!scm_is_eq (endianness, scm_i_native_endianness))       \
      c_result = INT_SWAP (_len) (c_result);                    \
                                                                \
    result = SCM_I_MAKINUM (c_result);                          \
  }                                                             \
                                                                \
  return result;

/* Template for fixed-size integer access using the native endianness.  */
#define INTEGER_NATIVE_REF(_len, _sign)			\
  SCM result;						\
							\
  INTEGER_GETTER_PROLOGUE (_len, _sign);                \
							\
  {							\
    INT_TYPE (_len, _sign)  c_result;			\
							\
    memcpy (&c_result, &c_bv[c_index], (_len) / 8);	\
    result = SCM_I_MAKINUM (c_result);			\
  }							\
							\
  return result;

/* Template for fixed-size integer modification (only 8, 16 or 32-bit).  */
#define INTEGER_SET(_len, _sign)				\
  INTEGER_SETTER_PROLOGUE (_len, _sign);                        \
  SCM_VALIDATE_SYMBOL (3, endianness);				\
								\
  {								\
    scm_t_signed_bits c_value;					\
    INT_TYPE (_len, _sign) c_value_short;			\
								\
    if (SCM_UNLIKELY (!SCM_I_INUMP (value)))			\
      scm_wrong_type_arg (FUNC_NAME, 3, value);			\
								\
    c_value = SCM_I_INUM (value);				\
    if (SCM_UNLIKELY (!INT_VALID_P (_len, _sign) (c_value)))	\
      scm_out_of_range (FUNC_NAME, value);			\
								\
    c_value_short = (INT_TYPE (_len, _sign)) c_value;		\
    if (!scm_is_eq (endianness, scm_i_native_endianness))       \
      c_value_short = INT_SWAP (_len) (c_value_short);		\
								\
    memcpy (&c_bv[c_index], &c_value_short, (_len) / 8);	\
  }								\
								\
  return SCM_UNSPECIFIED;

/* Template for fixed-size integer modification using the native
   endianness.  */
#define INTEGER_NATIVE_SET(_len, _sign)				\
  INTEGER_SETTER_PROLOGUE (_len, _sign);                        \
								\
  {								\
    scm_t_signed_bits c_value;					\
    INT_TYPE (_len, _sign) c_value_short;			\
								\
    if (SCM_UNLIKELY (!SCM_I_INUMP (value)))			\
      scm_wrong_type_arg (FUNC_NAME, 3, value);			\
								\
    c_value = SCM_I_INUM (value);				\
    if (SCM_UNLIKELY (!INT_VALID_P (_len, _sign) (c_value)))	\
      scm_out_of_range (FUNC_NAME, value);			\
								\
    c_value_short = (INT_TYPE (_len, _sign)) c_value;		\
								\
    memcpy (&c_bv[c_index], &c_value_short, (_len) / 8);	\
  }								\
								\
  return SCM_UNSPECIFIED;



/* Bytevector type.  */

#define SCM_BYTEVECTOR_HEADER_BYTES		\
  (SCM_BYTEVECTOR_HEADER_SIZE * sizeof (scm_t_bits))

#define SCM_BYTEVECTOR_SET_FLAG(bv, flag) \
  SCM_SET_BYTEVECTOR_FLAGS ((bv), SCM_BYTEVECTOR_FLAGS (bv) | flag)
#define SCM_BYTEVECTOR_SET_LENGTH(_bv, _len)            \
  SCM_SET_CELL_WORD_1 ((_bv), (scm_t_bits) (_len))
#define SCM_BYTEVECTOR_SET_CONTENTS(_bv, _contents)	\
  SCM_SET_CELL_WORD_2 ((_bv), (scm_t_bits) (_contents))
#define SCM_BYTEVECTOR_SET_PARENT(_bv, _parent)	\
  SCM_SET_CELL_OBJECT_3 ((_bv), (_parent))

#define SCM_VALIDATE_MUTABLE_BYTEVECTOR(pos, v) \
  SCM_MAKE_VALIDATE_MSG (pos, v, MUTABLE_BYTEVECTOR_P, "mutable bytevector")


/* The empty bytevector.  */
SCM scm_null_bytevector = SCM_UNSPECIFIED;


static inline SCM
make_bytevector (size_t len, scm_t_array_element_type element_type)
{
  SCM ret;
  size_t c_len;

  if (SCM_UNLIKELY (element_type > SCM_ARRAY_ELEMENT_TYPE_LAST
                    || scm_i_array_element_type_sizes[element_type] < 8))
    /* This would be an internal Guile programming error */
    abort ();

  /* Make sure that the total allocation size will not overflow size_t,
     with ~30 extra bytes to spare to avoid an overflow within the
     allocator.  */
  if (SCM_UNLIKELY (len >= (((size_t) -(SCM_BYTEVECTOR_HEADER_BYTES + 32))
                            / (scm_i_array_element_type_sizes[element_type]/8))))
    scm_num_overflow ("make-bytevector");

  if (SCM_UNLIKELY (len == 0 && element_type == SCM_ARRAY_ELEMENT_TYPE_VU8
		    && SCM_BYTEVECTOR_P (scm_null_bytevector)))
    ret = scm_null_bytevector;
  else
    {
      signed char *contents;

      c_len = len * (scm_i_array_element_type_sizes[element_type] / 8);

      contents = scm_gc_malloc_pointerless (SCM_BYTEVECTOR_HEADER_BYTES + c_len,
					    SCM_GC_BYTEVECTOR);
      ret = SCM_PACK_POINTER (contents);
      contents += SCM_BYTEVECTOR_HEADER_BYTES;

      SCM_SET_BYTEVECTOR_FLAGS (ret,
                                element_type | SCM_F_BYTEVECTOR_CONTIGUOUS);
      SCM_BYTEVECTOR_SET_LENGTH (ret, c_len);
      SCM_BYTEVECTOR_SET_CONTENTS (ret, contents);
      SCM_BYTEVECTOR_SET_PARENT (ret, SCM_BOOL_F);
    }

  return ret;
}

/* Return a bytevector of LEN elements of type ELEMENT_TYPE, with element
   values taken from CONTENTS.  Assume that the storage for CONTENTS will be
   automatically reclaimed when it becomes unreachable.  */
static inline SCM
make_bytevector_from_buffer (size_t len, void *contents,
			     scm_t_array_element_type element_type)
{
  SCM ret;

  if (SCM_UNLIKELY (len == 0))
    ret = make_bytevector (len, element_type);
  else
    {
      size_t c_len;

      ret = SCM_PACK_POINTER (scm_gc_malloc (SCM_BYTEVECTOR_HEADER_BYTES,
                                             SCM_GC_BYTEVECTOR));

      c_len = len * (scm_i_array_element_type_sizes[element_type] / 8);

      SCM_SET_BYTEVECTOR_FLAGS (ret, element_type);
      SCM_BYTEVECTOR_SET_LENGTH (ret, c_len);
      SCM_BYTEVECTOR_SET_CONTENTS (ret, contents);
      SCM_BYTEVECTOR_SET_PARENT (ret, SCM_BOOL_F);
    }

  return ret;
}


/* Return a new bytevector of size LEN octets.  */
SCM
scm_c_make_bytevector (size_t len)
{
  return make_bytevector (len, SCM_ARRAY_ELEMENT_TYPE_VU8);
}

/* Return a new bytevector of size LEN elements.  */
SCM
scm_i_make_typed_bytevector (size_t len, scm_t_array_element_type element_type)
{
  return make_bytevector (len, element_type);
}

/* Return a bytevector of size LEN made up of CONTENTS.  The area
   pointed to by CONTENTS must be protected from GC somehow: either
   because it was allocated using `scm_gc_malloc ()', or because it is
   part of PARENT.  */
SCM
scm_c_take_gc_bytevector (signed char *contents, size_t len, SCM parent)
{
  SCM ret;

  ret = make_bytevector_from_buffer (len, contents, SCM_ARRAY_ELEMENT_TYPE_VU8);
  SCM_BYTEVECTOR_SET_PARENT (ret, parent);

  return ret;
}

SCM
scm_c_take_typed_bytevector (signed char *contents, size_t len,
                             scm_t_array_element_type element_type, SCM parent)
{
  SCM ret;

  ret = make_bytevector_from_buffer (len, contents, element_type);
  SCM_BYTEVECTOR_SET_PARENT (ret, parent);

  return ret;
}

SCM_DEFINE (scm_bytevector_slice, "bytevector-slice", 2, 1, 0,
            (SCM bv, SCM offset, SCM size),
            "Return the slice of @var{bv} starting at @var{offset} and counting\n"
            "@var{size} bytes.  When @var{size} is omitted, the slice covers all\n"
            "of @var{bv} starting from @var{offset}.  The returned slice shares\n"
            "storage with @var{bv}: changes to the slice are visible in @var{bv}\n"
            "and vice-versa.\n"
            "\n"
            "When @var{bv} is actually a SRFI-4 uniform vector, its element\n"
            "type is preserved unless @var{offset} and @var{size} are not aligned\n"
            "on its element type size.\n")
#define FUNC_NAME s_scm_bytevector_slice
{
  SCM ret;
  size_t c_offset, c_size;
  scm_t_array_element_type element_type;

  SCM_VALIDATE_BYTEVECTOR (1, bv);

  c_offset = scm_to_size_t (offset);

  if (SCM_UNBNDP (size))
    {
      if (c_offset < SCM_BYTEVECTOR_LENGTH (bv))
        c_size = SCM_BYTEVECTOR_LENGTH (bv) - c_offset;
      else
        c_size = 0;
    }
  else
    c_size = scm_to_size_t (size);

  if (c_offset == 0 && c_size == SCM_BYTEVECTOR_LENGTH (bv))
    return bv;

  if (INT_ADD_OVERFLOW (c_offset, c_size)
      || (c_offset + c_size > SCM_BYTEVECTOR_LENGTH (bv)))
    scm_out_of_range (FUNC_NAME, offset);

  /* Preserve the element type of BV, unless we're not slicing on type
     boundaries.  */
  element_type = SCM_BYTEVECTOR_ELEMENT_TYPE (bv);
  if ((c_offset % SCM_BYTEVECTOR_TYPE_SIZE (bv) != 0)
      || (c_size % SCM_BYTEVECTOR_TYPE_SIZE (bv) != 0))
    element_type = SCM_ARRAY_ELEMENT_TYPE_VU8;
  else
    c_size /= (scm_i_array_element_type_sizes[element_type] / 8);

  ret = make_bytevector_from_buffer (c_size,
                                     SCM_BYTEVECTOR_CONTENTS (bv) + c_offset,
                                     element_type);
  if (!SCM_MUTABLE_BYTEVECTOR_P (bv))
    {
      /* Preserve the immutability property.  */
      scm_t_bits flags = SCM_BYTEVECTOR_FLAGS (ret);
      SCM_SET_BYTEVECTOR_FLAGS (ret, flags | SCM_F_BYTEVECTOR_IMMUTABLE);
    }

  SCM_BYTEVECTOR_SET_PARENT (ret, bv);

  return ret;
}
#undef FUNC_NAME

/* Shrink BV to C_NEW_LEN (which is assumed to be smaller than its current
   size) and return the new bytevector (possibly different from BV).  */
SCM
scm_c_shrink_bytevector (SCM bv, size_t c_new_len)
{
  SCM new_bv;
  size_t c_len;

  if (SCM_UNLIKELY (c_new_len % SCM_BYTEVECTOR_TYPE_SIZE (bv)))
    /* This would be an internal Guile programming error */
    abort ();

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  if (SCM_UNLIKELY (c_new_len > c_len))
    abort ();

  SCM_BYTEVECTOR_SET_LENGTH (bv, c_new_len);

  if (SCM_BYTEVECTOR_CONTIGUOUS_P (bv))
    {
      signed char *c_bv;

      c_bv = scm_gc_realloc (SCM2PTR (bv),
			     c_len + SCM_BYTEVECTOR_HEADER_BYTES,
			     c_new_len + SCM_BYTEVECTOR_HEADER_BYTES,
			     SCM_GC_BYTEVECTOR);
      new_bv = PTR2SCM (c_bv);
      SCM_BYTEVECTOR_SET_CONTENTS (new_bv, c_bv + SCM_BYTEVECTOR_HEADER_BYTES);
    }
  else
    {
      signed char *c_bv;

      c_bv = scm_gc_realloc (SCM_BYTEVECTOR_CONTENTS (bv),
			     c_len, c_new_len, SCM_GC_BYTEVECTOR);
      SCM_BYTEVECTOR_SET_CONTENTS (bv, c_bv);

      new_bv = bv;
    }

  return new_bv;
}

int
scm_is_bytevector (SCM obj)
{
  return SCM_BYTEVECTOR_P (obj);
}

size_t
scm_c_bytevector_length (SCM bv)
#define FUNC_NAME "scm_c_bytevector_length"
{
  SCM_VALIDATE_BYTEVECTOR (1, bv);

  return SCM_BYTEVECTOR_LENGTH (bv);
}
#undef FUNC_NAME

uint8_t
scm_c_bytevector_ref (SCM bv, size_t index)
#define FUNC_NAME "scm_c_bytevector_ref"
{
  size_t c_len;
  const uint8_t *c_bv;

  SCM_VALIDATE_BYTEVECTOR (1, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = (uint8_t *) SCM_BYTEVECTOR_CONTENTS (bv);

  if (SCM_UNLIKELY (index >= c_len))
    scm_out_of_range (FUNC_NAME, scm_from_size_t (index));

  return c_bv[index];
}
#undef FUNC_NAME

void
scm_c_bytevector_set_x (SCM bv, size_t index, uint8_t value)
#define FUNC_NAME "scm_c_bytevector_set_x"
{
  size_t c_len;
  uint8_t *c_bv;

  SCM_VALIDATE_MUTABLE_BYTEVECTOR (1, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = (uint8_t *) SCM_BYTEVECTOR_CONTENTS (bv);

  if (SCM_UNLIKELY (index >= c_len))
    scm_out_of_range (FUNC_NAME, scm_from_size_t (index));

  c_bv[index] = value;
}
#undef FUNC_NAME



int
scm_i_print_bytevector (SCM bv, SCM port, scm_print_state *pstate SCM_UNUSED)
{
  ssize_t ubnd, inc, i;
  scm_t_array_handle h;

  scm_array_get_handle (bv, &h);

  scm_putc ('#', port);
  scm_write (scm_array_handle_element_type (&h), port);
  scm_putc ('(', port);
  for (i = h.dims[0].lbnd, ubnd = h.dims[0].ubnd, inc = h.dims[0].inc;
       i <= ubnd; i += inc)
    {
      if (i > 0)
	scm_putc (' ', port);
      scm_write (scm_array_handle_ref (&h, i), port);
    }
  scm_putc (')', port);

  return 1;
}


/* General operations.  */

static SCM sym_big;
static SCM sym_little;

SCM scm_endianness_big, scm_endianness_little;

/* Host endianness (a symbol).  */
SCM scm_i_native_endianness = SCM_UNSPECIFIED;

/* Byte-swapping.  */
#ifndef bswap_24
# define bswap_24(_x)				\
  ((((_x) & 0xff0000) >> 16) |			\
   (((_x) & 0x00ff00))       |			\
   (((_x) & 0x0000ff) << 16))
#endif


SCM_DEFINE (scm_native_endianness, "native-endianness", 0, 0, 0,
	    (void),
	    "Return a symbol denoting the machine's native endianness.")
#define FUNC_NAME s_scm_native_endianness
{
  return scm_i_native_endianness;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_p, "bytevector?", 1, 0, 0,
	    (SCM obj),
	    "Return true if @var{obj} is a bytevector.")
#define FUNC_NAME s_scm_bytevector_p
{
  return scm_from_bool (scm_is_bytevector (obj));
}
#undef FUNC_NAME

SCM_DEFINE (scm_make_bytevector, "make-bytevector", 1, 1, 0,
	    (SCM len, SCM fill),
	    "Return a newly allocated bytevector of @var{len} bytes, "
	    "optionally filled with @var{fill}.")
#define FUNC_NAME s_scm_make_bytevector
{
  SCM bv;
  size_t c_len;
  uint8_t c_fill = 0;

  SCM_VALIDATE_SIZE_COPY (1, len, c_len);
  if (!scm_is_eq (fill, SCM_UNDEFINED))
    {
      int value;

      value = scm_to_int (fill);
      if (SCM_UNLIKELY ((value < -128) || (value > 255)))
	scm_out_of_range (FUNC_NAME, fill);
      c_fill = (uint8_t) value;
    }

  bv = make_bytevector (c_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
  if (!scm_is_eq (fill, SCM_UNDEFINED))
    {
      size_t i;
      uint8_t *contents;

      contents = (uint8_t *) SCM_BYTEVECTOR_CONTENTS (bv);
      for (i = 0; i < c_len; i++)
	contents[i] = c_fill;
    }
  else
    memset (SCM_BYTEVECTOR_CONTENTS (bv), 0, c_len);

  return bv;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_length, "bytevector-length", 1, 0, 0,
	    (SCM bv),
	    "Return the length (in bytes) of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_length
{
  return scm_from_size_t (scm_c_bytevector_length (bv));
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_eq_p, "bytevector=?", 2, 0, 0,
	    (SCM bv1, SCM bv2),
	    "Return is @var{bv1} equals to @var{bv2}---i.e., if they "
	    "have the same length and contents.")
#define FUNC_NAME s_scm_bytevector_eq_p
{
  SCM result = SCM_BOOL_F;
  size_t c_len1, c_len2;

  SCM_VALIDATE_BYTEVECTOR (1, bv1);
  SCM_VALIDATE_BYTEVECTOR (2, bv2);

  c_len1 = SCM_BYTEVECTOR_LENGTH (bv1);
  c_len2 = SCM_BYTEVECTOR_LENGTH (bv2);

  if (c_len1 == c_len2 && (SCM_BYTEVECTOR_ELEMENT_TYPE (bv1)
                           == SCM_BYTEVECTOR_ELEMENT_TYPE (bv2)))
    {
      signed char *c_bv1, *c_bv2;

      c_bv1 = SCM_BYTEVECTOR_CONTENTS (bv1);
      c_bv2 = SCM_BYTEVECTOR_CONTENTS (bv2);

      result = scm_from_bool (!memcmp (c_bv1, c_bv2, c_len1));
    }

  return result;
}
#undef FUNC_NAME

static SCM scm_bytevector_fill_partial_x (SCM bv, SCM fill, SCM start, SCM end);

SCM_DEFINE (scm_bytevector_fill_partial_x, "bytevector-fill!", 2, 2, 0,
	    (SCM bv, SCM fill, SCM start, SCM end),
	    "Fill positions [@var{start} ... @var{end}) of bytevector "
            "@var{bv} with @var{fill}, a byte. @var{start} defaults to 0 "
            "and @var{end} defaults to the length of @var{bv}. "
            "The return value is unspecified.")
#define FUNC_NAME s_scm_bytevector_fill_partial_x
{
  SCM_VALIDATE_MUTABLE_BYTEVECTOR (1, bv);

  int value  = scm_to_int (fill);
  if (SCM_UNLIKELY ((value < -128) || (value > 255)))
    scm_out_of_range (FUNC_NAME, fill);

  size_t i = 0;
  size_t c_end = SCM_BYTEVECTOR_LENGTH (bv);
  uint8_t *c_bv = (uint8_t *) SCM_BYTEVECTOR_CONTENTS (bv);

  if (!SCM_UNBNDP (start))
    i = scm_to_unsigned_integer (start, 0, c_end);
  if (!SCM_UNBNDP (end))
    c_end = scm_to_unsigned_integer (end, i, c_end);

  memset (c_bv + i, value, c_end-i);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM
scm_bytevector_fill_x (SCM bv, SCM fill)
#define FUNC_NAME s_scm_bytevector_fill_x
{
  return scm_bytevector_fill_partial_x (bv, fill, SCM_UNDEFINED, SCM_UNDEFINED);
}
#undef FUNC_NAME


SCM_DEFINE (scm_bytevector_copy_x, "bytevector-copy!", 5, 0, 0,
	    (SCM source, SCM source_start, SCM target, SCM target_start,
	     SCM len),
	    "Copy @var{len} bytes from @var{source} into @var{target}, "
	    "reading from a block starting at @var{source_start} (a positive "
            "index within @var{source}) and writing to a block starting at "
	    "@var{target_start}.\n\n"
            "It is permitted for the @var{source} and @var{target} regions to "
            "overlap. In that case, copying takes place as if the source is "
            "first copied into a temporary bytevector and then into the "
            "destination. ")
#define FUNC_NAME s_scm_bytevector_copy_x
{
  size_t c_len, c_source_len, c_target_len;
  size_t c_source_start, c_target_start;
  signed char *c_source, *c_target;

  SCM_VALIDATE_BYTEVECTOR (1, source);
  SCM_VALIDATE_MUTABLE_BYTEVECTOR (3, target);

  c_len = scm_to_size_t (len);
  c_source_start = scm_to_size_t (source_start);
  c_target_start = scm_to_size_t (target_start);

  c_source = SCM_BYTEVECTOR_CONTENTS (source);
  c_target = SCM_BYTEVECTOR_CONTENTS (target);
  c_source_len = SCM_BYTEVECTOR_LENGTH (source);
  c_target_len = SCM_BYTEVECTOR_LENGTH (target);

  if (SCM_UNLIKELY (c_source_len < c_source_start
                    || (c_source_len - c_source_start < c_len)))
    scm_out_of_range (FUNC_NAME, source_start);
  if (SCM_UNLIKELY (c_target_len < c_target_start
                    || (c_target_len - c_target_start < c_len)))
    scm_out_of_range (FUNC_NAME, target_start);

  memmove (c_target + c_target_start,
	   c_source + c_source_start,
	   c_len);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_copy, "bytevector-copy", 1, 0, 0,
	    (SCM bv),
	    "Return a newly allocated copy of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_copy
{
  SCM copy;
  size_t c_len;
  signed char *c_bv, *c_copy;

  SCM_VALIDATE_BYTEVECTOR (1, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = SCM_BYTEVECTOR_CONTENTS (bv);

  copy = make_bytevector (c_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
  c_copy = SCM_BYTEVECTOR_CONTENTS (copy);
  memcpy (c_copy, c_bv, c_len);

  return copy;
}
#undef FUNC_NAME

SCM_DEFINE (scm_uniform_array_to_bytevector, "uniform-array->bytevector",
            1, 0, 0, (SCM array),
	    "Return a newly allocated bytevector whose contents\n"
            "will be copied from the uniform array @var{array}.")
#define FUNC_NAME s_scm_uniform_array_to_bytevector
{
  SCM contents, ret;
  size_t len, sz, byte_len;
  scm_t_array_handle h;
  const void *elts;

  contents = scm_array_contents (array, SCM_BOOL_T);
  if (scm_is_false (contents))
    scm_wrong_type_arg_msg (FUNC_NAME, 0, array, "uniform contiguous array");

  scm_array_get_handle (contents, &h);
  assert (h.base == 0);

  elts = h.elements;
  len = h.dims->inc * (h.dims->ubnd - h.dims->lbnd + 1);
  sz = scm_array_handle_uniform_element_bit_size (&h);
  if (sz >= 8 && ((sz % 8) == 0))
    byte_len = len * (sz / 8);
  else if (sz < 8)
    /* Elements of sub-byte size (bitvectors) are addressed in 32-bit
       units.  */
    byte_len = ((len * sz + 31) / 32) * 4;
  else
    /* an internal guile error, really */
    SCM_MISC_ERROR ("uniform elements larger than 8 bits must fill whole bytes", SCM_EOL);

  ret = make_bytevector (byte_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
  if (byte_len != 0)
    /* Empty arrays may have elements == NULL.  We must avoid passing
       NULL to memcpy, even if the length is zero, to avoid undefined
       behavior. */
    memcpy (SCM_BYTEVECTOR_CONTENTS (ret), elts, byte_len);

  scm_array_handle_release (&h);

  return ret;
}
#undef FUNC_NAME


/* Operations on bytes and octets.  */

SCM_DEFINE (scm_bytevector_u8_ref, "bytevector-u8-ref", 2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the octet located at @var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_u8_ref
{
  INTEGER_NATIVE_REF (8, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s8_ref, "bytevector-s8-ref", 2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the byte located at @var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_s8_ref
{
  INTEGER_NATIVE_REF (8, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u8_set_x, "bytevector-u8-set!", 3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Return the octet located at @var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_u8_set_x
{
  INTEGER_NATIVE_SET (8, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s8_set_x, "bytevector-s8-set!", 3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Return the octet located at @var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_s8_set_x
{
  INTEGER_NATIVE_SET (8, signed);
}
#undef FUNC_NAME


SCM_DEFINE (scm_bytevector_to_u8_list, "bytevector->u8-list", 1, 0, 0,
	    (SCM bv),
	    "Return a newly allocated list of octets containing the "
	    "contents of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_to_u8_list
{
  SCM lst, pair;
  size_t c_len, i;
  uint8_t *c_bv;

  SCM_VALIDATE_BYTEVECTOR (1, bv);

  c_len = SCM_BYTEVECTOR_LENGTH (bv);
  c_bv = (uint8_t *) SCM_BYTEVECTOR_CONTENTS (bv);

  lst = scm_make_list (scm_from_size_t (c_len), SCM_UNSPECIFIED);
  for (i = 0, pair = lst;
       i < c_len;
       i++, pair = SCM_CDR (pair))
    {
      SCM_SETCAR (pair, SCM_I_MAKINUM (c_bv[i]));
    }

  return lst;
}
#undef FUNC_NAME

SCM_DEFINE (scm_u8_list_to_bytevector, "u8-list->bytevector", 1, 0, 0,
	    (SCM lst),
	    "Turn @var{lst}, a list of octets, into a bytevector.")
#define FUNC_NAME s_scm_u8_list_to_bytevector
{
  SCM bv, item;
  size_t c_len, i;
  uint8_t *c_bv;

  SCM_VALIDATE_LIST_COPYLEN (1, lst, c_len);

  bv = make_bytevector (c_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
  c_bv = (uint8_t *) SCM_BYTEVECTOR_CONTENTS (bv);

  for (i = 0; i < c_len; lst = SCM_CDR (lst), i++)
    {
      item = SCM_CAR (lst);

      if (SCM_LIKELY (SCM_I_INUMP (item)))
	{
	  scm_t_signed_bits c_item;

	  c_item = SCM_I_INUM (item);
	  if (SCM_LIKELY ((c_item >= 0) && (c_item < 256)))
	    c_bv[i] = (uint8_t) c_item;
	  else
	    goto type_error;
	}
      else
	goto type_error;
    }

  return bv;

 type_error:
  scm_wrong_type_arg (FUNC_NAME, 1, item);

  return SCM_BOOL_F;
}
#undef FUNC_NAME

/* Compute the two's complement of VALUE (a positive integer) on SIZE octets
   using (2^(SIZE * 8) - VALUE).  */
static inline void
twos_complement (mpz_t value, size_t size)
{
  uintptr_t bit_count;

  /* We expect BIT_COUNT to fit in a uintptr_t thanks to the range
     checking on SIZE performed earlier.  */
  bit_count = (uintptr_t) size << 3ULL;

  if (SCM_LIKELY (bit_count < sizeof (uintptr_t)))
    mpz_ui_sub (value, 1ULL << bit_count, value);
  else
    {
      mpz_t max;

      mpz_init (max);
      mpz_ui_pow_ui (max, 2, bit_count);
      mpz_sub (value, max, value);
      mpz_clear (max);
    }
}

static inline SCM
bytevector_large_ref (const char *c_bv, size_t c_size, int signed_p,
		      SCM endianness)
{
  SCM result;
  mpz_t c_mpz;
  int c_endianness, negative_p = 0;

  if (signed_p)
    {
      if (scm_is_eq (endianness, sym_big))
	negative_p = c_bv[0] & 0x80;
      else
	negative_p = c_bv[c_size - 1] & 0x80;
    }

  c_endianness = scm_is_eq (endianness, sym_big) ? 1 : -1;

  mpz_init (c_mpz);
  mpz_import (c_mpz, 1 /* 1 word */, 1 /* word order doesn't matter */,
	      c_size /* word is C_SIZE-byte long */,
	      c_endianness,
	      0 /* nails */, c_bv);

  if (signed_p && negative_p)
    {
      twos_complement (c_mpz, c_size);
      mpz_neg (c_mpz, c_mpz);
    }

  result = scm_from_mpz (c_mpz);
  mpz_clear (c_mpz);  /* FIXME: Needed? */

  return result;
}

static inline int
bytevector_large_set (char *c_bv, size_t c_size, int signed_p,
		      SCM value, SCM endianness)
{
  mpz_t c_mpz;
  int c_endianness, c_sign, err = 0;

  c_endianness = scm_is_eq (endianness, sym_big) ? 1 : -1;

  mpz_init (c_mpz);
  scm_to_mpz (value, c_mpz);

  c_sign = mpz_sgn (c_mpz);
  if (c_sign < 0)
    {
      if (SCM_LIKELY (signed_p))
	{
	  mpz_neg (c_mpz, c_mpz);
	  twos_complement (c_mpz, c_size);
	}
      else
	{
	  err = -1;
	  goto finish;
	}
    }

  if (c_sign == 0)
    /* Zero.  */
    memset (c_bv, 0, c_size);
  else
    {
      size_t word_count, value_words;

      value_words = ((mpz_sizeinbase (c_mpz, 2) + (8 * c_size) - 1) /
                     (8 * c_size));
      if (SCM_UNLIKELY (value_words > 1))
	{
	  err = -2;
	  goto finish;
	}


      mpz_export (c_bv, &word_count, 1 /* word order doesn't matter */,
		  c_size, c_endianness,
		  0 /* nails */, c_mpz);
      if (SCM_UNLIKELY (word_count != 1))
	/* Shouldn't happen since we already checked with VALUE_SIZE.  */
	abort ();
    }

 finish:
  mpz_clear (c_mpz);

  return err;
}

#define GENERIC_INTEGER_ACCESSOR_PROLOGUE(validate, _sign)              \
  size_t c_len, c_index, c_size;					\
  char *c_bv;								\
									\
  SCM_VALIDATE_##validate (1, bv);					\
  c_index = scm_to_size_t (index);					\
  c_size = scm_to_size_t (size);					\
									\
  c_len = SCM_BYTEVECTOR_LENGTH (bv);					\
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);				\
									\
  /* C_SIZE must have its 3 higher bits set to zero so that		\
     multiplying it by 8 yields a number that fits in a			\
     size_t.  */							\
  if (SCM_UNLIKELY (c_size == 0 || c_size >= (SIZE_MAX >> 3)))		\
    scm_out_of_range (FUNC_NAME, size);					\
  if (SCM_UNLIKELY (c_len < c_index					\
                    || (c_len - c_index < c_size)))			\
    scm_out_of_range (FUNC_NAME, index);

#define GENERIC_INTEGER_GETTER_PROLOGUE(_sign)          \
  GENERIC_INTEGER_ACCESSOR_PROLOGUE (BYTEVECTOR, _sign)
#define GENERIC_INTEGER_SETTER_PROLOGUE(_sign)                  \
  GENERIC_INTEGER_ACCESSOR_PROLOGUE (MUTABLE_BYTEVECTOR, _sign)

/* Template of an integer reference function.  */
#define GENERIC_INTEGER_REF(_sign)					\
  SCM result;								\
									\
  if (c_size < 3)							\
    {									\
      int swap;								\
      _sign int value;							\
									\
      swap = !scm_is_eq (endianness, scm_i_native_endianness);		\
      switch (c_size)							\
	{								\
	case 1:								\
	  {								\
	    _sign char c_value8;					\
	    memcpy (&c_value8, c_bv, 1);				\
	    value = c_value8;						\
	  }								\
	  break;							\
	case 2:								\
	  {								\
	    INT_TYPE (16, _sign)  c_value16;				\
	    memcpy (&c_value16, c_bv, 2);				\
	    if (swap)							\
	      value = (INT_TYPE (16, _sign)) bswap_16 (c_value16);	\
	    else							\
	      value = c_value16;					\
	  }								\
	  break;							\
	default:							\
	  abort ();							\
	}								\
									\
      result = SCM_I_MAKINUM ((_sign int) value);			\
    }									\
  else									\
    result = bytevector_large_ref ((char *) c_bv,			\
				   c_size, SIGNEDNESS (_sign),		\
				   endianness);				\
									\
  return result;

static inline SCM
bytevector_signed_ref (const char *c_bv, size_t c_size, SCM endianness)
{
  GENERIC_INTEGER_REF (signed);
}

static inline SCM
bytevector_unsigned_ref (const char *c_bv, size_t c_size, SCM endianness)
{
  GENERIC_INTEGER_REF (unsigned);
}


/* Template of an integer assignment function.  */
#define GENERIC_INTEGER_SET(_sign)					\
  if (c_size < 3)							\
    {									\
      scm_t_signed_bits c_value;					\
									\
      if (SCM_UNLIKELY (!SCM_I_INUMP (value)))				\
	goto range_error;						\
									\
      c_value = SCM_I_INUM (value);					\
      switch (c_size)							\
	{								\
	case 1:								\
	  if (SCM_LIKELY (INT_VALID_P (8, _sign) (c_value)))		\
	    {								\
	      _sign char c_value8;					\
	      c_value8 = (_sign char) c_value;				\
	      memcpy (c_bv, &c_value8, 1);				\
	    }								\
	  else								\
	    goto range_error;						\
	  break;							\
									\
	case 2:								\
	  if (SCM_LIKELY (INT_VALID_P (16, _sign) (c_value)))		\
	    {								\
	      int swap;							\
	      INT_TYPE (16, _sign)  c_value16;				\
									\
	      swap = !scm_is_eq (endianness, scm_i_native_endianness);	\
									\
	      if (swap)							\
		c_value16 = (INT_TYPE (16, _sign)) bswap_16 (c_value);	\
	      else							\
		c_value16 = c_value;					\
									\
	      memcpy (c_bv, &c_value16, 2);				\
	    }								\
	  else								\
	    goto range_error;						\
	  break;							\
									\
	default:							\
	  abort ();							\
	}								\
    }									\
  else									\
    {									\
      int err;								\
									\
      err = bytevector_large_set (c_bv, c_size,				\
				  SIGNEDNESS (_sign),			\
				  value, endianness);			\
      if (err)								\
	goto range_error;						\
    }									\
									\
  return;								\
									\
 range_error:								\
  scm_out_of_range (FUNC_NAME, value);					\
  return;

static inline void
bytevector_signed_set (char *c_bv, size_t c_size,
		       SCM value, SCM endianness,
		       const char *func_name)
#define FUNC_NAME func_name
{
  GENERIC_INTEGER_SET (signed);
}
#undef FUNC_NAME

static inline void
bytevector_unsigned_set (char *c_bv, size_t c_size,
			 SCM value, SCM endianness,
			 const char *func_name)
#define FUNC_NAME func_name
{
  GENERIC_INTEGER_SET (unsigned);
}
#undef FUNC_NAME

#undef GENERIC_INTEGER_SET
#undef GENERIC_INTEGER_REF


SCM_DEFINE (scm_bytevector_uint_ref, "bytevector-uint-ref", 4, 0, 0,
	    (SCM bv, SCM index, SCM endianness, SCM size),
	    "Return the @var{size}-octet long unsigned integer at index "
	    "@var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_uint_ref
{
  GENERIC_INTEGER_GETTER_PROLOGUE (unsigned);

  return (bytevector_unsigned_ref (&c_bv[c_index], c_size, endianness));
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_sint_ref, "bytevector-sint-ref", 4, 0, 0,
	    (SCM bv, SCM index, SCM endianness, SCM size),
	    "Return the @var{size}-octet long unsigned integer at index "
	    "@var{index} in @var{bv}.")
#define FUNC_NAME s_scm_bytevector_sint_ref
{
  GENERIC_INTEGER_GETTER_PROLOGUE (signed);

  return (bytevector_signed_ref (&c_bv[c_index], c_size, endianness));
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_uint_set_x, "bytevector-uint-set!", 5, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness, SCM size),
	    "Set the @var{size}-octet long unsigned integer at @var{index} "
	    "to @var{value}.")
#define FUNC_NAME s_scm_bytevector_uint_set_x
{
  GENERIC_INTEGER_SETTER_PROLOGUE (unsigned);

  bytevector_unsigned_set (&c_bv[c_index], c_size, value, endianness,
			   FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_sint_set_x, "bytevector-sint-set!", 5, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness, SCM size),
	    "Set the @var{size}-octet long signed integer at @var{index} "
	    "to @var{value}.")
#define FUNC_NAME s_scm_bytevector_sint_set_x
{
  GENERIC_INTEGER_SETTER_PROLOGUE (signed);

  bytevector_signed_set (&c_bv[c_index], c_size, value, endianness,
			 FUNC_NAME);

  return SCM_UNSPECIFIED;
}
#undef FUNC_NAME



/* Operations on integers of arbitrary size.  */

#define INTEGERS_TO_LIST(_sign)						\
  SCM lst, pair;							\
  size_t i, c_len, c_size;						\
									\
  SCM_VALIDATE_BYTEVECTOR (1, bv);					\
  SCM_VALIDATE_SYMBOL (2, endianness);					\
  c_size = scm_to_unsigned_integer (size, 1, (size_t) -1);		\
									\
  c_len = SCM_BYTEVECTOR_LENGTH (bv);					\
  if (SCM_UNLIKELY (c_len % c_size != 0))				\
    scm_wrong_type_arg_msg						\
      (FUNC_NAME, 0, size,						\
       "an exact positive integer that divides the bytevector length");	\
  else if (SCM_UNLIKELY (c_len == 0))					\
    lst = SCM_EOL;							\
  else									\
    {									\
      const char *c_bv;							\
									\
      c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);			\
									\
      lst = scm_make_list (scm_from_size_t (c_len / c_size),		\
			   SCM_UNSPECIFIED);				\
      for (i = 0, pair = lst;						\
	   i <= c_len - c_size;						\
	   i += c_size, c_bv += c_size, pair = SCM_CDR (pair))		\
	{								\
	  SCM_SETCAR (pair,						\
		      bytevector_ ## _sign ## _ref (c_bv, c_size,	\
						    endianness));	\
	}								\
    }									\
									\
  return lst;

SCM_DEFINE (scm_bytevector_to_sint_list, "bytevector->sint-list",
	    3, 0, 0,
	    (SCM bv, SCM endianness, SCM size),
	    "Return a list of signed integers of @var{size} octets "
	    "representing the contents of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_to_sint_list
{
  INTEGERS_TO_LIST (signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_to_uint_list, "bytevector->uint-list",
	    3, 0, 0,
	    (SCM bv, SCM endianness, SCM size),
	    "Return a list of unsigned integers of @var{size} octets "
	    "representing the contents of @var{bv}.")
#define FUNC_NAME s_scm_bytevector_to_uint_list
{
  INTEGERS_TO_LIST (unsigned);
}
#undef FUNC_NAME

#undef INTEGER_TO_LIST


#define INTEGER_LIST_TO_BYTEVECTOR(_sign)				\
  SCM bv;								\
  size_t c_len;								\
  size_t c_size;							\
  char *c_bv, *c_bv_ptr;						\
									\
  SCM_VALIDATE_LIST_COPYLEN (1, lst, c_len);				\
  SCM_VALIDATE_SYMBOL (2, endianness);					\
  c_size = scm_to_size_t (size);					\
									\
  if (SCM_UNLIKELY (c_size == 0 || c_size >= (SIZE_MAX >> 3)))		\
    scm_out_of_range (FUNC_NAME, size);					\
									\
  bv = make_bytevector (c_len * c_size, SCM_ARRAY_ELEMENT_TYPE_VU8);    \
  c_bv = (char *) SCM_BYTEVECTOR_CONTENTS (bv);				\
									\
  for (c_bv_ptr = c_bv;							\
       !scm_is_null (lst);						\
       lst = SCM_CDR (lst), c_bv_ptr += c_size)				\
    {									\
      bytevector_ ## _sign ## _set (c_bv_ptr, c_size,			\
				    SCM_CAR (lst), endianness,		\
				    FUNC_NAME);				\
    }									\
									\
  return bv;


SCM_DEFINE (scm_uint_list_to_bytevector, "uint-list->bytevector",
	    3, 0, 0,
	    (SCM lst, SCM endianness, SCM size),
	    "Return a bytevector containing the unsigned integers "
	    "listed in @var{lst} and encoded on @var{size} octets "
	    "according to @var{endianness}.")
#define FUNC_NAME s_scm_uint_list_to_bytevector
{
  INTEGER_LIST_TO_BYTEVECTOR (unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_sint_list_to_bytevector, "sint-list->bytevector",
	    3, 0, 0,
	    (SCM lst, SCM endianness, SCM size),
	    "Return a bytevector containing the signed integers "
	    "listed in @var{lst} and encoded on @var{size} octets "
	    "according to @var{endianness}.")
#define FUNC_NAME s_scm_sint_list_to_bytevector
{
  INTEGER_LIST_TO_BYTEVECTOR (signed);
}
#undef FUNC_NAME

#undef INTEGER_LIST_TO_BYTEVECTOR



/* Operations on 16-bit integers.  */

SCM_DEFINE (scm_bytevector_u16_ref, "bytevector-u16-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the unsigned 16-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_u16_ref
{
  INTEGER_REF (16, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s16_ref, "bytevector-s16-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the signed 16-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_s16_ref
{
  INTEGER_REF (16, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u16_native_ref, "bytevector-u16-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 16-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u16_native_ref
{
  INTEGER_NATIVE_REF (16, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s16_native_ref, "bytevector-s16-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 16-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s16_native_ref
{
  INTEGER_NATIVE_REF (16, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u16_set_x, "bytevector-u16-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_u16_set_x
{
  INTEGER_SET (16, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s16_set_x, "bytevector-s16-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_s16_set_x
{
  INTEGER_SET (16, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u16_native_set_x, "bytevector-u16-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the unsigned integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u16_native_set_x
{
  INTEGER_NATIVE_SET (16, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s16_native_set_x, "bytevector-s16-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the signed integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s16_native_set_x
{
  INTEGER_NATIVE_SET (16, signed);
}
#undef FUNC_NAME



/* Operations on 32-bit integers.  */

/* Unfortunately, on 32-bit machines `SCM' is not large enough to hold
   arbitrary 32-bit integers.  Thus we fall back to using the
   `large_{ref,set}' variants on 32-bit machines.  */

#define LARGE_INTEGER_REF(_len, _sign)					\
  INTEGER_GETTER_PROLOGUE(_len, _sign);                                 \
  SCM_VALIDATE_SYMBOL (3, endianness);					\
									\
  return (bytevector_large_ref ((char *) c_bv + c_index, _len / 8,	\
				SIGNEDNESS (_sign), endianness));

#define LARGE_INTEGER_SET(_len, _sign)					\
  int err;								\
  INTEGER_SETTER_PROLOGUE (_len, _sign);                                \
  SCM_VALIDATE_SYMBOL (4, endianness);					\
									\
  err = bytevector_large_set ((char *) c_bv + c_index, _len / 8,	\
			      SIGNEDNESS (_sign), value, endianness);	\
  if (SCM_UNLIKELY (err))						\
     scm_out_of_range (FUNC_NAME, value);				\
									\
  return SCM_UNSPECIFIED;

#define LARGE_INTEGER_NATIVE_REF(_len, _sign)                           \
  INTEGER_GETTER_PROLOGUE(_len, _sign);                                 \
  return (bytevector_large_ref ((char *) c_bv + c_index, _len / 8,      \
				SIGNEDNESS (_sign), scm_i_native_endianness));

#define LARGE_INTEGER_NATIVE_SET(_len, _sign)				\
  int err;								\
  INTEGER_SETTER_PROLOGUE (_len, _sign);                                \
									\
  err = bytevector_large_set ((char *) c_bv + c_index, _len / 8,	\
			      SIGNEDNESS (_sign), value,		\
			      scm_i_native_endianness);			\
  if (SCM_UNLIKELY (err))						\
     scm_out_of_range (FUNC_NAME, value);				\
									\
  return SCM_UNSPECIFIED;


SCM_DEFINE (scm_bytevector_u32_ref, "bytevector-u32-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the unsigned 32-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_u32_ref
{
#if SIZEOF_VOID_P > 4
  INTEGER_REF (32, unsigned);
#else
  LARGE_INTEGER_REF (32, unsigned);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s32_ref, "bytevector-s32-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the signed 32-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_s32_ref
{
#if SIZEOF_VOID_P > 4
  INTEGER_REF (32, signed);
#else
  LARGE_INTEGER_REF (32, signed);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u32_native_ref, "bytevector-u32-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 32-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u32_native_ref
{
#if SIZEOF_VOID_P > 4
  INTEGER_NATIVE_REF (32, unsigned);
#else
  LARGE_INTEGER_NATIVE_REF (32, unsigned);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s32_native_ref, "bytevector-s32-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 32-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s32_native_ref
{
#if SIZEOF_VOID_P > 4
  INTEGER_NATIVE_REF (32, signed);
#else
  LARGE_INTEGER_NATIVE_REF (32, signed);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u32_set_x, "bytevector-u32-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_u32_set_x
{
#if SIZEOF_VOID_P > 4
  INTEGER_SET (32, unsigned);
#else
  LARGE_INTEGER_SET (32, unsigned);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s32_set_x, "bytevector-s32-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_s32_set_x
{
#if SIZEOF_VOID_P > 4
  INTEGER_SET (32, signed);
#else
  LARGE_INTEGER_SET (32, signed);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u32_native_set_x, "bytevector-u32-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the unsigned integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u32_native_set_x
{
#if SIZEOF_VOID_P > 4
  INTEGER_NATIVE_SET (32, unsigned);
#else
  LARGE_INTEGER_NATIVE_SET (32, unsigned);
#endif
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s32_native_set_x, "bytevector-s32-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the signed integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s32_native_set_x
{
#if SIZEOF_VOID_P > 4
  INTEGER_NATIVE_SET (32, signed);
#else
  LARGE_INTEGER_NATIVE_SET (32, signed);
#endif
}
#undef FUNC_NAME



/* Operations on 64-bit integers.  */

/* For 64-bit integers, we use only the `large_{ref,set}' variant.  */

SCM_DEFINE (scm_bytevector_u64_ref, "bytevector-u64-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the unsigned 64-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_u64_ref
{
  LARGE_INTEGER_REF (64, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s64_ref, "bytevector-s64-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the signed 64-bit integer from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_s64_ref
{
  LARGE_INTEGER_REF (64, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u64_native_ref, "bytevector-u64-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 64-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u64_native_ref
{
  LARGE_INTEGER_NATIVE_REF (64, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s64_native_ref, "bytevector-s64-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the unsigned 64-bit integer from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s64_native_ref
{
  LARGE_INTEGER_NATIVE_REF (64, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u64_set_x, "bytevector-u64-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_u64_set_x
{
  LARGE_INTEGER_SET (64, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s64_set_x, "bytevector-s64-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_s64_set_x
{
  LARGE_INTEGER_SET (64, signed);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_u64_native_set_x, "bytevector-u64-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the unsigned integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_u64_native_set_x
{
  LARGE_INTEGER_NATIVE_SET (64, unsigned);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_s64_native_set_x, "bytevector-s64-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the signed integer @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_s64_native_set_x
{
  LARGE_INTEGER_NATIVE_SET (64, signed);
}
#undef FUNC_NAME



/* Operations on IEEE-754 numbers.  */

/* There are two possible word endians, visible in glibc's <ieee754.h>.
   However, in R6RS, when the endianness is `little', little endian is
   assumed for both the byte order and the word order.  This is clear from
   Section 2.1 of R6RS-lib (in response to
   http://www.r6rs.org/formal-comments/comment-187.txt).  */

union scm_ieee754_float
{
  float f;
  uint32_t i;
};

union scm_ieee754_double
{
  double d;
  uint64_t i;
};


/* Convert to/from a floating-point number with different endianness.  This
   method is probably not the most efficient but it should be portable.  */

static inline void
float_to_foreign_endianness (union scm_ieee754_float *target,
			     float source)
{
  union scm_ieee754_float input;

  input.f = source;
  target->i = bswap_32 (input.i);
}

static inline float
float_from_foreign_endianness (const union scm_ieee754_float *source)
{
  union scm_ieee754_float result;

  result.i = bswap_32 (source->i);

  return (result.f);
}

static inline void
double_to_foreign_endianness (union scm_ieee754_double *target,
			      double source)
{
  union scm_ieee754_double input;

  input.d = source;
  target->i = bswap_64 (input.i);
}

static inline double
double_from_foreign_endianness (const union scm_ieee754_double *source)
{
  union scm_ieee754_double result;

  result.i = bswap_64 (source->i);

  return (result.d);
}

/* Template macros to abstract over doubles and floats.
   XXX: Guile can only convert to/from doubles.  */
#define IEEE754_UNION(_c_type)           union scm_ieee754_ ## _c_type
#define IEEE754_TO_SCM(_c_type)          scm_from_double
#define IEEE754_FROM_SCM(_c_type)        scm_to_double
#define IEEE754_FROM_FOREIGN_ENDIANNESS(_c_type)	\
   _c_type ## _from_foreign_endianness
#define IEEE754_TO_FOREIGN_ENDIANNESS(_c_type)	\
   _c_type ## _to_foreign_endianness


/* FIXME: SCM_VALIDATE_REAL rejects integers, etc. grrr */
#define VALIDATE_REAL(pos, v) \
  do { \
    SCM_ASSERT_TYPE (scm_is_real (v), v, pos, FUNC_NAME, "real"); \
  } while (0)

/* Templace getters and setters.  */

#define IEEE754_GETTER_PROLOGUE(_type)                          \
  INTEGER_GETTER_PROLOGUE (sizeof (_type) << 3UL, signed);

#define IEEE754_SETTER_PROLOGUE(_type)                          \
  INTEGER_SETTER_PROLOGUE (sizeof (_type) << 3UL, signed);

#define IEEE754_REF(_type)					\
  _type c_result;						\
								\
  IEEE754_GETTER_PROLOGUE (_type);                              \
  SCM_VALIDATE_SYMBOL (3, endianness);				\
								\
  if (scm_is_eq (endianness, scm_i_native_endianness))		\
    memcpy (&c_result, &c_bv[c_index], sizeof (c_result));	\
  else								\
    {								\
      IEEE754_UNION (_type) c_raw;				\
								\
      memcpy (&c_raw, &c_bv[c_index], sizeof (c_raw));		\
      c_result =						\
	IEEE754_FROM_FOREIGN_ENDIANNESS (_type) (&c_raw);	\
    }								\
								\
  return (IEEE754_TO_SCM (_type) (c_result));

#define IEEE754_NATIVE_REF(_type)				\
  _type c_result;						\
								\
  IEEE754_GETTER_PROLOGUE (_type);				\
								\
  memcpy (&c_result, &c_bv[c_index], sizeof (c_result));	\
  return (IEEE754_TO_SCM (_type) (c_result));

#define IEEE754_SET(_type)					\
  _type c_value;						\
								\
  IEEE754_SETTER_PROLOGUE (_type);				\
  VALIDATE_REAL (3, value);					\
  SCM_VALIDATE_SYMBOL (4, endianness);				\
  c_value = IEEE754_FROM_SCM (_type) (value);			\
								\
  if (scm_is_eq (endianness, scm_i_native_endianness))		\
    memcpy (&c_bv[c_index], &c_value, sizeof (c_value));	\
  else								\
    {								\
      IEEE754_UNION (_type) c_raw;				\
								\
      IEEE754_TO_FOREIGN_ENDIANNESS (_type) (&c_raw, c_value);	\
      memcpy (&c_bv[c_index], &c_raw, sizeof (c_raw));		\
    }								\
								\
  return SCM_UNSPECIFIED;

#define IEEE754_NATIVE_SET(_type)			\
  _type c_value;					\
							\
  IEEE754_SETTER_PROLOGUE (_type);			\
  VALIDATE_REAL (3, value);				\
  c_value = IEEE754_FROM_SCM (_type) (value);		\
							\
  memcpy (&c_bv[c_index], &c_value, sizeof (c_value));	\
  return SCM_UNSPECIFIED;


/* Single precision.  */

SCM_DEFINE (scm_bytevector_ieee_single_ref,
	    "bytevector-ieee-single-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the IEEE-754 single from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_ieee_single_ref
{
  IEEE754_REF (float);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_single_native_ref,
	    "bytevector-ieee-single-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the IEEE-754 single from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_ieee_single_native_ref
{
  IEEE754_NATIVE_REF (float);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_single_set_x,
	    "bytevector-ieee-single-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store real @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_ieee_single_set_x
{
  IEEE754_SET (float);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_single_native_set_x,
	    "bytevector-ieee-single-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the real @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_ieee_single_native_set_x
{
  IEEE754_NATIVE_SET (float);
}
#undef FUNC_NAME


/* Double precision.  */

SCM_DEFINE (scm_bytevector_ieee_double_ref,
	    "bytevector-ieee-double-ref",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM endianness),
	    "Return the IEEE-754 double from @var{bv} at "
	    "@var{index}.")
#define FUNC_NAME s_scm_bytevector_ieee_double_ref
{
  IEEE754_REF (double);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_double_native_ref,
	    "bytevector-ieee-double-native-ref",
	    2, 0, 0,
	    (SCM bv, SCM index),
	    "Return the IEEE-754 double from @var{bv} at "
	    "@var{index} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_ieee_double_native_ref
{
  IEEE754_NATIVE_REF (double);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_double_set_x,
	    "bytevector-ieee-double-set!",
	    4, 0, 0,
	    (SCM bv, SCM index, SCM value, SCM endianness),
	    "Store real @var{value} in @var{bv} at @var{index} according to "
	    "@var{endianness}.")
#define FUNC_NAME s_scm_bytevector_ieee_double_set_x
{
  IEEE754_SET (double);
}
#undef FUNC_NAME

SCM_DEFINE (scm_bytevector_ieee_double_native_set_x,
	    "bytevector-ieee-double-native-set!",
	    3, 0, 0,
	    (SCM bv, SCM index, SCM value),
	    "Store the real @var{value} at index @var{index} "
	    "of @var{bv} using the native endianness.")
#define FUNC_NAME s_scm_bytevector_ieee_double_native_set_x
{
  IEEE754_NATIVE_SET (double);
}
#undef FUNC_NAME


#undef IEEE754_UNION
#undef IEEE754_TO_SCM
#undef IEEE754_FROM_SCM
#undef IEEE754_FROM_FOREIGN_ENDIANNESS
#undef IEEE754_TO_FOREIGN_ENDIANNESS
#undef IEEE754_REF
#undef IEEE754_NATIVE_REF
#undef IEEE754_SET
#undef IEEE754_NATIVE_SET


/* Operations on strings.  */


/* Produce a function that returns the length of a UTF-encoded string.  */
#define UTF_STRLEN_FUNCTION(_utf_width)					\
static inline size_t							\
utf ## _utf_width ## _strlen (const uint ## _utf_width ## _t *str)	\
{									\
  size_t len = 0;							\
  const uint ## _utf_width ## _t *ptr;					\
  for (ptr = str;							\
       *ptr != 0;							\
       ptr++)								\
    {									\
      len++;								\
    }									\
									\
  return (len * ((_utf_width) / 8));					\
}

UTF_STRLEN_FUNCTION (8)


/* Return the length (in bytes) of STR, a UTF-(UTF_WIDTH) encoded string.  */
#define UTF_STRLEN(_utf_width, _str)		\
  utf ## _utf_width ## _strlen (_str)

/* Return the "portable" name of the UTF encoding of size UTF_WIDTH and
   ENDIANNESS (Gnulib's `iconv_open' module guarantees the portability of the
   encoding name).  */
static inline void
utf_encoding_name (char *name, size_t utf_width, SCM endianness)
{
  strcpy (name, "UTF-");
  strcat (name, ((utf_width == 8)
		 ? "8"
		 : ((utf_width == 16)
		    ? "16"
		    : ((utf_width == 32)
		       ? "32"
		       : "??"))));
  strcat (name,
	  ((scm_is_eq (endianness, sym_big))
	   ? "BE"
	   : ((scm_is_eq (endianness, sym_little))
	      ? "LE"
	      : "unknown")));
}

/* Maximum length of a UTF encoding name.  */
#define MAX_UTF_ENCODING_NAME_LEN  16

/* Produce the body of a `string->utf' function.  */
#define STRING_TO_UTF(_utf_width)                                       \
  SCM utf;                                                              \
  int err;                                                              \
  char c_utf_name[MAX_UTF_ENCODING_NAME_LEN];                           \
  char *c_utf = NULL;                                                   \
  size_t c_strlen, c_utf_len = 0;                                       \
                                                                        \
  SCM_VALIDATE_STRING (1, str);                                         \
  if (scm_is_eq (endianness, SCM_UNDEFINED))                            \
    endianness = sym_big;                                               \
  else                                                                  \
    SCM_VALIDATE_SYMBOL (2, endianness);                                \
                                                                        \
  utf_encoding_name (c_utf_name, (_utf_width), endianness);             \
                                                                        \
  c_strlen = scm_i_string_length (str);                                 \
  if (scm_i_is_narrow_string (str))                                     \
    {                                                                   \
      err = mem_iconveh (scm_i_string_chars (str), c_strlen,            \
                         "ISO-8859-1", c_utf_name,                      \
                         iconveh_question_mark, NULL,                   \
                         &c_utf, &c_utf_len);                           \
      if (SCM_UNLIKELY (err))                                           \
        scm_syserror_msg (FUNC_NAME, "failed to convert string: ~A",    \
                          scm_list_1 (str), err);                       \
    }                                                                   \
  else                                                                  \
    {                                                                   \
      const scm_t_wchar *wbuf = scm_i_string_wide_chars (str);          \
      c_utf = u32_conv_to_encoding (c_utf_name,                         \
                                    iconveh_question_mark,              \
                                    (uint32_t *) wbuf,                  \
                                    c_strlen, NULL, NULL, &c_utf_len);  \
      if (SCM_UNLIKELY (c_utf == NULL))                                 \
        scm_syserror_msg (FUNC_NAME, "failed to convert string: ~A",    \
                          scm_list_1 (str), errno);                     \
    }                                                                   \
  scm_dynwind_begin (0);                                                \
  scm_dynwind_free (c_utf);                                             \
  utf = make_bytevector (c_utf_len, SCM_ARRAY_ELEMENT_TYPE_VU8);        \
  memcpy (SCM_BYTEVECTOR_CONTENTS (utf), c_utf, c_utf_len);             \
  scm_dynwind_end ();                                                   \
                                                                        \
  return (utf);



SCM_DEFINE (scm_string_to_utf8, "string->utf8",
	    1, 0, 0,
	    (SCM str),
	    "Return a newly allocated bytevector that contains the UTF-8 "
	    "encoding of @var{str}.")
#define FUNC_NAME s_scm_string_to_utf8
{
  SCM utf;
  uint8_t *c_utf;
  size_t c_utf_len = 0;

  SCM_VALIDATE_STRING (1, str);

  c_utf = (uint8_t *) scm_to_utf8_stringn (str, &c_utf_len);
  utf = make_bytevector (c_utf_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
  memcpy (SCM_BYTEVECTOR_CONTENTS (utf), c_utf, c_utf_len);
  free (c_utf);

  return (utf);
}
#undef FUNC_NAME

SCM_DEFINE (scm_string_to_utf16, "string->utf16",
	    1, 1, 0,
	    (SCM str, SCM endianness),
	    "Return a newly allocated bytevector that contains the UTF-16 "
	    "encoding of @var{str}.")
#define FUNC_NAME s_scm_string_to_utf16
{
  STRING_TO_UTF (16);
}
#undef FUNC_NAME

static void
swap_u32 (scm_t_wchar *vals, size_t len)
{
  size_t n;
  for (n = 0; n < len; n++)
    vals[n] = bswap_32 (vals[n]);
}

SCM_DEFINE (scm_string_to_utf32, "string->utf32",
	    1, 1, 0,
	    (SCM str, SCM endianness),
	    "Return a newly allocated bytevector that contains the UTF-32 "
	    "encoding of @var{str}.")
#define FUNC_NAME s_scm_string_to_utf32
{
  SCM bv;
  scm_t_wchar *wchars;
  size_t wchar_len, bytes_len;

  wchars = scm_to_utf32_stringn (str, &wchar_len);
  bytes_len = wchar_len * sizeof (scm_t_wchar);
  if (!scm_is_eq (SCM_UNBNDP (endianness) ? scm_endianness_big : endianness,
                  scm_i_native_endianness))
    swap_u32 (wchars, wchar_len);

  bv = make_bytevector (bytes_len, SCM_ARRAY_ELEMENT_TYPE_VU8);
  memcpy (SCM_BYTEVECTOR_CONTENTS (bv), wchars, bytes_len);
  free (wchars);

  return bv;
}
#undef FUNC_NAME


/* Produce the body of a function that converts a UTF-encoded bytevector to a
   string.  */
#define UTF_TO_STRING(_utf_width)					\
  SCM str = SCM_BOOL_F;							\
  int err;								\
  char *c_str = NULL;                                                   \
  char c_utf_name[MAX_UTF_ENCODING_NAME_LEN];				\
  char *c_utf;                                                          \
  size_t c_strlen = 0, c_utf_len = 0;					\
									\
  SCM_VALIDATE_BYTEVECTOR (1, utf);					\
  if (scm_is_eq (endianness, SCM_UNDEFINED))                            \
    endianness = sym_big;						\
  else									\
    SCM_VALIDATE_SYMBOL (2, endianness);				\
									\
  c_utf_len = SCM_BYTEVECTOR_LENGTH (utf);				\
  c_utf = (char *) SCM_BYTEVECTOR_CONTENTS (utf);			\
  utf_encoding_name (c_utf_name, (_utf_width), endianness);		\
									\
  err = mem_iconveh (c_utf, c_utf_len,					\
		     c_utf_name, "UTF-8",				\
		     iconveh_question_mark, NULL,			\
		     &c_str, &c_strlen);				\
  if (SCM_UNLIKELY (err))						\
    scm_syserror_msg (FUNC_NAME, "failed to convert to string: ~A",	\
		      scm_list_1 (utf), err);				\
  else									\
    {                                                                   \
      str = scm_from_utf8_stringn (c_str, c_strlen);                    \
      free (c_str);                                                     \
    }                                                                   \
  return (str);


SCM_DEFINE (scm_utf8_to_string, "utf8->string",
	    1, 0, 0,
	    (SCM utf),
	    "Return a newly allocate string that contains from the UTF-8-"
	    "encoded contents of bytevector @var{utf}.")
#define FUNC_NAME s_scm_utf8_to_string
{
  SCM str;
  const char *c_utf;
  size_t c_utf_len = 0;

  SCM_VALIDATE_BYTEVECTOR (1, utf);

  c_utf_len = SCM_BYTEVECTOR_LENGTH (utf);
  c_utf = (char *) SCM_BYTEVECTOR_CONTENTS (utf);
  str = scm_from_utf8_stringn (c_utf, c_utf_len);

  return (str);
}
#undef FUNC_NAME

SCM_DEFINE (scm_utf16_to_string, "utf16->string",
	    1, 1, 0,
	    (SCM utf, SCM endianness),
	    "Return a newly allocate string that contains from the UTF-16-"
	    "encoded contents of bytevector @var{utf}.")
#define FUNC_NAME s_scm_utf16_to_string
{
  UTF_TO_STRING (16);
}
#undef FUNC_NAME

SCM_DEFINE (scm_utf32_to_string, "utf32->string",
	    1, 1, 0,
	    (SCM utf, SCM endianness),
	    "Return a newly allocate string that contains from the UTF-32-"
	    "encoded contents of bytevector @var{utf}.")
#define FUNC_NAME s_scm_utf32_to_string
{
  UTF_TO_STRING (32);
}
#undef FUNC_NAME


/* Initialization.  */

void
scm_bootstrap_bytevectors (void)
{
  /* This must be instantiated here because the generalized-vector API may
     want to access bytevectors even though `(rnrs bytevectors)' hasn't been
     loaded.  */
  scm_null_bytevector = make_bytevector (0, SCM_ARRAY_ELEMENT_TYPE_VU8);


  scm_endianness_big = sym_big = scm_from_latin1_symbol ("big");
  scm_endianness_little = sym_little = scm_from_latin1_symbol ("little");

#ifdef WORDS_BIGENDIAN
  scm_i_native_endianness = sym_big;
#else
  scm_i_native_endianness = sym_little;
#endif

  scm_c_register_extension ("libguile-" SCM_EFFECTIVE_VERSION,
                            "scm_init_bytevectors",
			    (scm_t_extension_init_func) scm_init_bytevectors,
			    NULL);

  scm_i_register_vector_constructor
    (scm_i_array_element_types[SCM_ARRAY_ELEMENT_TYPE_VU8],
     scm_make_bytevector);
}

void
scm_init_bytevectors (void)
{
#include "bytevectors.x"
}
