# Copyright (C) 2002-2020 Free Software Foundation, Inc.
#
# This file is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this file.  If not, see <https://www.gnu.org/licenses/>.
#
# As a special exception to the GNU General Public License,
# this file may be distributed as part of a program that
# contains a configuration script generated by Autoconf, under
# the same distribution terms as the rest of that program.
#
# Generated by gnulib-tool.
#
# This file represents the specification of how gnulib-tool is used.
# It acts as a cache: It is written and read by gnulib-tool.
# In projects that use version control, this file is meant to be put under
# version control, like the configure.ac and various Makefile.am files.


# Specification in the form of a command-line invocation:
# gnulib-tool --import --local-dir=gnulib-local \
#  --lib=libgnu \
#  --source-base=lib \
#  --m4-base=m4 \
#  --doc-base=doc \
#  --tests-base=tests \
#  --aux-dir=build-aux \
#  --lgpl=3 \
#  --conditional-dependencies \
#  --libtool \
#  --macro-prefix=gl \
#  --no-vc-files \
#  --avoid=lock \
#  --avoid=unistr/base \
#  --avoid=unistr/u8-mbtouc \
#  --avoid=unistr/u8-mbtouc-unsafe \
#  --avoid=unistr/u8-mbtoucr \
#  --avoid=unistr/u8-prev \
#  --avoid=unistr/u8-uctomb \
#  --avoid=unitypes \
#  accept4 \
#  alignof \
#  alloca-opt \
#  announce-gen \
#  autobuild \
#  bind \
#  byteswap \
#  c-strcase \
#  canonicalize-lgpl \
#  ceil \
#  clock-time \
#  close \
#  connect \
#  copysign \
#  dirfd \
#  dirname-lgpl \
#  duplocale \
#  environ \
#  extensions \
#  flock \
#  floor \
#  fpieee \
#  frexp \
#  fstat \
#  fsync \
#  full-read \
#  full-write \
#  func \
#  gendocs \
#  getaddrinfo \
#  getlogin \
#  getpeername \
#  getsockname \
#  getsockopt \
#  git-version-gen \
#  gitlog-to-changelog \
#  gnu-web-doc-update \
#  gnupload \
#  havelib \
#  iconv_open-utf \
#  inet_ntop \
#  inet_pton \
#  isfinite \
#  isinf \
#  isnan \
#  ldexp \
#  lib-symbol-versions \
#  lib-symbol-visibility \
#  libunistring \
#  link \
#  listen \
#  localcharset \
#  locale \
#  log1p \
#  lstat \
#  maintainer-makefile \
#  malloc-gnu \
#  malloca \
#  mkdir \
#  mkostemp \
#  nl_langinfo \
#  nproc \
#  open \
#  pipe-posix \
#  pipe2 \
#  poll \
#  putenv \
#  readlink \
#  recv \
#  recvfrom \
#  regex \
#  rename \
#  rmdir \
#  select \
#  send \
#  sendto \
#  setenv \
#  setsockopt \
#  shutdown \
#  socket \
#  stat-time \
#  stdlib \
#  strftime \
#  striconveh \
#  string \
#  sys_stat \
#  time \
#  times \
#  trunc \
#  unistd \
#  verify \
#  vsnprintf \
#  warnings \
#  wchar

# Specification in the form of a few gnulib-tool.m4 macro invocations:
gl_LOCAL_DIR([gnulib-local])
gl_MODULES([
  accept4
  alignof
  alloca-opt
  announce-gen
  autobuild
  bind
  byteswap
  c-strcase
  canonicalize-lgpl
  ceil
  clock-time
  close
  connect
  copysign
  dirfd
  dirname-lgpl
  duplocale
  environ
  extensions
  flock
  floor
  fpieee
  frexp
  fstat
  fsync
  full-read
  full-write
  func
  gendocs
  getaddrinfo
  getlogin
  getpeername
  getsockname
  getsockopt
  git-version-gen
  gitlog-to-changelog
  gnu-web-doc-update
  gnupload
  havelib
  iconv_open-utf
  inet_ntop
  inet_pton
  isfinite
  isinf
  isnan
  ldexp
  lib-symbol-versions
  lib-symbol-visibility
  libunistring
  link
  listen
  localcharset
  locale
  log1p
  lstat
  maintainer-makefile
  malloc-gnu
  malloca
  mkdir
  mkostemp
  nl_langinfo
  nproc
  open
  pipe-posix
  pipe2
  poll
  putenv
  readlink
  recv
  recvfrom
  regex
  rename
  rmdir
  select
  send
  sendto
  setenv
  setsockopt
  shutdown
  socket
  stat-time
  stdlib
  strftime
  striconveh
  string
  sys_stat
  time
  times
  trunc
  unistd
  verify
  vsnprintf
  warnings
  wchar
])
gl_AVOID([lock unistr/base unistr/u8-mbtouc unistr/u8-mbtouc-unsafe unistr/u8-mbtoucr unistr/u8-prev unistr/u8-uctomb unitypes])
gl_SOURCE_BASE([lib])
gl_M4_BASE([m4])
gl_PO_BASE([])
gl_DOC_BASE([doc])
gl_TESTS_BASE([tests])
gl_LIB([libgnu])
gl_LGPL([3])
gl_MAKEFILE_NAME([])
gl_CONDITIONAL_DEPENDENCIES
gl_LIBTOOL
gl_MACRO_PREFIX([gl])
gl_PO_DOMAIN([])
gl_WITNESS_C_MACRO([])
gl_VC_FILES([false])
