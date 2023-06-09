## Process this file with automake to produce Makefile.in.
##
##  	Copyright (C) 2011, 2023 Free Software Foundation, Inc.
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

EXTRA_DIST +=					\
  %D%/gc-profile.scm				\
  %D%/gcbench.scm				\
  %D%/guile-test.scm				\
  %D%/loop.scm					\
  %D%/run-benchmark.scm				\
  %D%/string.scm				\
  $(gc_benchmarks)

# GPLv2+ Larceny GC benchmarks by Lars Hansen et al. from
# <http://www.ccs.neu.edu/home/will/GC/sourcecode.html>.
gc_benchmarks =					\
  %D%/larceny/GPL				\
  %D%/larceny/README				\
  %D%/larceny/dumb.sch				\
  %D%/larceny/dummy.sch				\
  %D%/larceny/dynamic-input-large.sch		\
  %D%/larceny/dynamic-input-small.sch		\
  %D%/larceny/dynamic.sch			\
  %D%/larceny/earley.sch			\
  %D%/larceny/gcbench.sch			\
  %D%/larceny/gcold.scm				\
  %D%/larceny/graphs.sch			\
  %D%/larceny/lattice.sch			\
  %D%/larceny/nboyer.sch			\
  %D%/larceny/nucleic2.sch			\
  %D%/larceny/perm.sch				\
  %D%/larceny/run-benchmark.chez		\
  %D%/larceny/sboyer.sch			\
  %D%/larceny/softscheme.sch			\
  %D%/larceny/twobit-input-long.sch		\
  %D%/larceny/twobit-input-short.sch		\
  %D%/larceny/twobit-smaller.sch		\
  %D%/larceny/twobit.sch
