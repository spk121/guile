## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 1998, 1999, 2000, 2001, 2002, 2006, 2007,
##        2008, 2009, 2010, 2011, 2012, 2013,
##        2014, 2015, 2016, 2022, 2023 Free Software Foundation, Inc.
##
##   This file is part of GUILE.
##
##   GUILE is free software; you can redistribute it and/or modify it
##   under the terms of the GNU Lesser General Public License as
##   published by the Free Software Foundation; either version 3, or
##   (at your option) any later version.
##
##   GUILE is distributed in the hope that it will be useful, but
##   WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU Lesser General Public License for more details.
##
##   You should have received a copy of the GNU Lesser General Public
##   License along with GUILE; see the file COPYING.LESSER.  If not,
##   write to the Free Software Foundation, Inc., 51 Franklin Street,
##   Fifth Floor, Boston, MA 02110-1301 USA

# want automake 1.10 or higher so that AM_GNU_GETTEXT can tell automake that
# config.rpath is needed
#
AUTOMAKE_OPTIONS = 1.10

# The test-suite goes last so that it can depend on everything else
# (e.g. guile-procedures.txt).
SUBDIRS =					\
	lib					\
	meta					\
	libguile				\
	module					\
	stage0					\
	stage1					\
	stage2					\
	guile-readline				\
	examples				\
	doc					\
	.					\
	test-suite

DIST_SUBDIRS = $(SUBDIRS) prebuilt

libguileincludedir = $(pkgincludedir)/$(GUILE_EFFECTIVE_VERSION)
libguileinclude_HEADERS = libguile.h

schemelibdir = $(pkgdatadir)/$(GUILE_EFFECTIVE_VERSION)
schemelib_DATA = libguile/guile-procedures.txt

# Our own implementation of Gnulib's lock interface.
noinst_HEADERS = lib/glthread/lock.h

# Our lib/glthreads/lock.h header indirectly includes
# libguile/scmconfig.h.  Make sure it is built before we recurse into
# lib/.
BUILT_SOURCES = libguile/scmconfig.h
libguile/scmconfig.h:
	$(MAKE) -C libguile scmconfig.h

# Build it here so that (given SUBDIRS order) the modules are available
libguile/guile-procedures.txt:
	$(MAKE) -C libguile guile-procedures.txt

EXTRA_DIST = LICENSE HACKING GUILE-VERSION			\
	     am/maintainer-dirs					\
	     am/guilec						\
	     am/ChangeLog-2008					\
	     m4/ChangeLog-2008					\
	     m4/gnulib-cache.m4					\
	     ChangeLog-2008					\
	     .version						\
	     gnulib-local/lib/localcharset.h.diff		\
	     gnulib-local/lib/localcharset.c.diff		\
	     gnulib-local/m4/clock_time.m4.diff			\
	     gnulib-local/build-aux/git-version-gen.diff	\
	     libguile/texi-fragments-to-docstrings		\
	     gdbinit						\
	     guix.scm						\
	     .guix/modules/guile-package.scm			\
	     .guix/manifest.scm

ACLOCAL_AMFLAGS = -I m4

DISTCLEANFILES = check-guile.log

DISTCHECK_CONFIGURE_FLAGS = --enable-error-on-warning --enable-mini-gmp

dist-hook: gen-ChangeLog gen-tarball-version		\
  assert-no-store-file-names ensure-guile-readline

# 'guile-readline/Makefile.am' conditionally includes various files to
# the distribution.  Bail out when 'HAVE_READLINE' is false.
if HAVE_READLINE
ensure-guile-readline:
	@true
else !HAVE_READLINE
ensure-guile-readline:
	@echo "error: Readline support is required for 'make dist'." >&2
	@exit 1
endif !HAVE_READLINE
.PHONY: ensure-guile-readline

clean-local:
	rm -rf cache/

gen_start_rev = 61db429e251bfd2f75cb4632972e0238056eb24b
.PHONY: gen-ChangeLog
gen-ChangeLog:
	if test -d .git; then						\
	  $(top_srcdir)/build-aux/gitlog-to-changelog			\
	    $(gen_start_rev)..HEAD > $(distdir)/cl-t;			\
	  rm -f $(distdir)/ChangeLog;					\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;			\
	fi

# Make sure we're not shipping a file that embeds a /gnu/store file
# name, for maintainers who use Guix.
.PHONY: assert-no-store-file-names
assert-no-store-file-names:
	if grep --exclude=ChangeLog -rE "/gnu/store/[a-z0-9]{32}-" $(distdir) ;			\
	then									\
	  echo "error: store file names embedded in the distribution" >&2 ;	\
	  exit 1 ;								\
	fi

BUILT_SOURCES += $(top_srcdir)/.version
$(top_srcdir)/.version: Makefile
	echo $(VERSION) > $@-t && mv $@-t $@
gen-tarball-version:
	echo $(VERSION) > $(distdir)/.tarball-version

include benchmark-suite/local.mk
include gc-benchmarks/local.mk

# Makefile.am ends here
