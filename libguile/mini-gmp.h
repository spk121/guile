#ifndef SCM_MINI_GMP_H
#define SCM_MINI_GMP_H

/* Copyright 2026
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



/* On Windows, long is always 32 bits even on 64-bit. Default to long long
 * unless the user is compiling Guile with an explicit MINI_GMP_LIMB_TYPE.
 */
#if !defined(MINI_GMP_LIMB_TYPE) && defined(_WIN64)
#define MINI_GMP_LIMB_TYPE long long
#endif

#include "mini-gmp/mini-gmp.h"

#endif  /* SCM_MINI_GMP_H */
