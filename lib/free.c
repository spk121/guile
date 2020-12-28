/* Make free() preserve errno.

   Copyright (C) 2003, 2006, 2009-2020 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU Lesser General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* written by Paul Eggert */

#include <config.h>

#include <stdlib.h>

#include <errno.h>

void
rpl_free (void *p)
#undef free
{
  int err = errno;
  free (p);
  errno = err;
}
