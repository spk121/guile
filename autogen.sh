#!/bin/sh
# Usage: sh -x ./autogen.sh

set -e

[ -f GUILE-VERSION ] || {
  echo "autogen.sh: run this command only at the top of guile-core."
  exit 1
}

######################################################################
### update infrastructure

autoreconf -i --force --verbose

# Patch build-aux/ltmain.sh to apply fix for handling arguments
# correctly when invoking Windows executables.  This patch comes from
# libtool's Git repository:
# http://git.savannah.gnu.org/cgit/libtool.git/commit/?id=101ad44541c6d303cf465937a212042885f4338e
patch -p1 < ltmain.sh.patch

echo "Now run configure and make."
