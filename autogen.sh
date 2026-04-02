#!/bin/sh
# Usage: sh -x ./autogen.sh

set -e

[ -f GUILE-VERSION ] || {
  echo "autogen.sh: run this command only at the top of guile-core."
  exit 1
}

######################################################################
### announce build tool versions
echo ""
autoconf --version
echo ""
automake --version
echo ""

# Typical MacOS X installations rename 'libtoolize' to 'glibtoolize', so
# adjust to that.
if test "`uname -s`" = "Darwin"; then
  glibtoolize --version
else
  libtoolize --version
fi

echo ""
${M4:-m4} --version
echo ""
flex --version
echo ""

######################################################################
### Extract the serial number from an m4 file.
### The serial line must appear before the second newline (i.e. within
### the first two lines), and must match exactly:
###   serial NNN
### where NNN is 1-3 decimal digits. Anything else yields 0.
get_serial() {
  awk '
    {
      if (match($0, /^[\t]*#.* serial [0-9][0-9]?[0-9]?/)) {
        s = substr($0, RSTART, RLENGTH)
        n = split(s, a, " ")
        print a[n]
        found = 1
        exit
      }
      lines++
    }
    lines >= 2 { print 0; found = 1; exit }
    END         { if (!found) print 0 }
  ' "$1"
}

######################################################################
### update infrastructure

M4_DIR="m4"

# Step 1: Back up the current m4 directory before autopoint touches it.
M4_BAK=$(mktemp -d)
trap 'rm -rf "$M4_BAK"' EXIT
cp "$M4_DIR"/*.m4 "$M4_BAK"/

# Step 2: Run autopoint for real, so it can do its full job on po/,
# intl/, and any other infrastructure it manages — not just m4 files.
autopoint --force

# Step 3: Restore any m4 file from the backup whose serial is greater
# than (or equal to, meaning autopoint downgraded it) what autopoint
# just installed.  If our backup had no serial (0) and autopoint also
# wrote no serial (0) we leave autopoint's version in place, since the
# backup and the installed file are from the same generation.
for bak_file in "$M4_BAK"/*.m4; do
  [ -f "$bak_file" ] || continue
  base=$(basename "$bak_file")
  our_file="$M4_DIR/$base"

  bak_serial=$(get_serial "$bak_file")

  if [ -f "$our_file" ]; then
    ap_serial=$(get_serial "$our_file")
  else
    ap_serial=0
  fi

  if [ "$bak_serial" -gt "$ap_serial" ]; then
    echo "autogen.sh: restoring $our_file (serial $ap_serial from autopoint < backed-up serial $bak_serial)"
    cp "$bak_file" "$our_file"
  fi
done

# Step 4: Run autoreconf, suppressing its internal autopoint call since
# we have already run it above and protected the m4 directory.
AUTOPOINT=true autoreconf -vif

echo "Now run configure and make."
