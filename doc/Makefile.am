## Process this file with Automake to create Makefile.in
##
##     Copyright (C) 1998, 2002, 2006, 2008-2010 Free Software Foundation, Inc.
##     Copyright (C) 2014, 2025 Free Software Foundation, Inc.
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

AUTOMAKE_OPTIONS = gnu

SUBDIRS = ref r5rs

guile_generated_mans = guile.1 guile-config.1 guile-snarf.1

dist_man1_MANS = $(guile_generated_mans)

EXTRA_DIST = groupings.alist ChangeLog-2008 # guile-api.alist
EXTRA_DIST += $(guile_generated_mans:.1=.1.in)

CLEANFILES = $(guile_generated_mans)

include $(top_srcdir)/am/maintainer-dirs
guile-api.alist: guile-api.alist-FORCE
	( cd $(top_builddir) ; $(mscripts)/update-guile-api.alist )
guile-api.alist-FORCE:

$(guile_generated_mans): %.1: %.1.in $(top_srcdir)/.version
	$(SED) -e "s/\@GUILE_VERSION\@/$(GUILE_VERSION)/" $< > $@.tmp
	mv $@.tmp $@
