# unidata_to_charset.awk --- Compute SRFI-14 charsets from UnicodeData.txt
#
# Copyright (C) 2009, 2010, 2022 Free Software Foundation, Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this library; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

# Utilities
###########

# Print MESSAGE to standard error, and exit with STATUS.
function die(status, message) {
    print "unidata_to_charset.awk:", message | "cat 1>&2";
    exit_status = status;
    exit exit_status;
}

# Parse the string S as a hexadecimal number.  Note that R, C, and B are
# local variables that need not be set by callers.  Most Awk
# implementations have an 'strtonum' function that we could use, but it
# is not part of POSIX.
function hex(s, r, c, b) {
    if (length(s) == 0) {
        die(1, "Cannot parse empty string as hexadecimal.");
    }
    r = 0;
    for (i = 1; i <= length(s); i++) {
        c = substr(s, i, 1);
        b = 0;
        if      (c == "0") { b =  0; }
        else if (c == "1") { b =  1; }
        else if (c == "2") { b =  2; }
        else if (c == "3") { b =  3; }
        else if (c == "4") { b =  4; }
        else if (c == "5") { b =  5; }
        else if (c == "6") { b =  6; }
        else if (c == "7") { b =  7; }
        else if (c == "8") { b =  8; }
        else if (c == "9") { b =  9; }
        else if (c == "A") { b = 10; }
        else if (c == "B") { b = 11; }
        else if (c == "C") { b = 12; }
        else if (c == "D") { b = 13; }
        else if (c == "E") { b = 14; }
        else if (c == "F") { b = 15; }
        else { die(1, "Invalid hexadecimal character: " c); }
        r *= 16;
        r += b;
    }
    return r;
}

# Program initialization
########################

BEGIN {
    # The columns are separated by semicolons.
    FS = ";";

    # This will help us handle errors.
    exit_status = 0;

    # List of charsets.
    all_charsets_count = 0;
    all_charsets[all_charsets_count++] = "lower_case";
    all_charsets[all_charsets_count++] = "upper_case";
    all_charsets[all_charsets_count++] = "title_case";
    all_charsets[all_charsets_count++] = "letter";
    all_charsets[all_charsets_count++] = "digit";
    all_charsets[all_charsets_count++] = "hex_digit";
    all_charsets[all_charsets_count++] = "letter_plus_digit";
    all_charsets[all_charsets_count++] = "graphic";
    all_charsets[all_charsets_count++] = "whitespace";
    all_charsets[all_charsets_count++] = "printing";
    all_charsets[all_charsets_count++] = "iso_control";
    all_charsets[all_charsets_count++] = "punctuation";
    all_charsets[all_charsets_count++] = "symbol";
    all_charsets[all_charsets_count++] = "blank";
    all_charsets[all_charsets_count++] = "ascii";
    all_charsets[all_charsets_count++] = "empty";
    all_charsets[all_charsets_count++] = "designated";

    # Initialize charset state table.
    for (i in all_charsets) {
        cs = all_charsets[i];
        state[cs, "start"] = -1;
        state[cs, "end"] = -1;
        state[cs, "count"] = 0;
    }
}

# Comments
##########

# Skip comments so we can include a copyright notice in the data file.
/^#/ {
    next;
}

# Record initialization
#######################

# In this block we give names to each field, and do some basic
# initialization.
{
    codepoint = hex($1);
    name = $2;
    category = $3;
    uppercase = $13;
    lowercase = $14;

    codepoint_end = codepoint;
    charset_count = 0;
}

# Some pairs of lines in UnicodeData.txt delimit ranges of
# characters.
name ~ /First>$/ {
    getline;
    last_name = name;
    sub(/First>$/, "Last>", last_name);
    if (last_name != $2) {
        die(1, "Invalid range in Unicode data.");
        exit_status = 1;
        exit 1;
    }
    codepoint_end = hex($1);
}

# Character set predicates
##########################

## The lower_case character set
###############################

# For Unicode, we follow Java's specification: a character is
# lowercase if
#    * it is not in the range [U+2000,U+2FFF] ([8192,12287]), and
#    * the Unicode attribute table does not give a lowercase mapping
#      for it, and
#    * at least one of the following is true:
#          o the Unicode attribute table gives a mapping to uppercase
#            for the character, or
#          o the name for the character in the Unicode attribute table
#            contains the words "SMALL LETTER" or "SMALL LIGATURE".

(codepoint < 8192 || codepoint > 12287) &&
lowercase == "" &&
(uppercase != "" || name ~ /(SMALL LETTER|SMALL LIGATURE)/) {
    charsets[charset_count++] = "lower_case";
}

## The upper_case character set
###############################

# For Unicode, we follow Java's specification: a character is
# uppercase if
#    * it is not in the range [U+2000,U+2FFF] ([8192,12287]), and
#    * the Unicode attribute table does not give an uppercase mapping
#      for it (this excludes titlecase characters), and
#    * at least one of the following is true:
#          o the Unicode attribute table gives a mapping to lowercase
#            for the character, or
#          o the name for the character in the Unicode attribute table
#            contains the words "CAPITAL LETTER" or "CAPITAL LIGATURE".

(codepoint < 8192 || codepoint > 12287) &&
uppercase == "" &&
(lowercase != "" || name ~ /(CAPITAL LETTER|CAPITAL LIGATURE)/) {
    charsets[charset_count++] = "upper_case";
}

## The title_case character set
###############################

# A character is titlecase if it has the category Lt in the character
# attribute database.

category == "Lt" {
    charsets[charset_count++] = "title_case";
}

## The letter character set
###########################

# A letter is any character with one of the letter categories (Lu, Ll,
# Lt, Lm, Lo) in the Unicode character database.

category == "Lu" ||
category == "Ll" ||
category == "Lt" ||
category == "Lm" ||
category == "Lo" {
    charsets[charset_count++] = "letter";
    charsets[charset_count++] = "letter_plus_digit";
}

## The digit character set
##########################

# A character is a digit if it has the category Nd in the character
# attribute database. In Latin-1 and ASCII, the only such characters
# are 0123456789. In Unicode, there are other digit characters in
# other code blocks, such as Gujarati digits and Tibetan digits.

category == "Nd" {
    charsets[charset_count++] = "digit";
    charsets[charset_count++] = "letter_plus_digit";
}

## The hex_digit character set
##############################

# The only hex digits are 0123456789abcdefABCDEF.

(codepoint >= 48 && codepoint <= 57) ||
(codepoint >= 65 && codepoint <= 70) ||
(codepoint >= 97 && codepoint <= 102) {
    charsets[charset_count++] = "hex_digit";
}

## The graphic character set
############################

# Characters that would 'use ink' when printed

category ~ /L|M|N|P|S/ {
    charsets[charset_count++] = "graphic";
    charsets[charset_count++] = "printing";
}

## The whitespace character set
###############################

# A whitespace character is either
#    * a character with one of the space, line, or paragraph separator
#      categories (Zs, Zl or Zp) of the Unicode character database.
#    * U+0009 (09) Horizontal tabulation (\t control-I)
#    * U+000A (10) Line feed (\n control-J)
#    * U+000B (11) Vertical tabulation (\v control-K)
#    * U+000C (12) Form feed (\f control-L)
#    * U+000D (13) Carriage return (\r control-M)

category ~ /Zs|Zl|Zp/ ||
(codepoint >= 9 && codepoint <= 13) {
    charsets[charset_count++] = "whitespace";
    charsets[charset_count++] = "printing";
}

## The iso_control character set
################################

# The ISO control characters are the Unicode/Latin-1 characters in the
# ranges [U+0000,U+001F] ([0,31]) and [U+007F,U+009F] ([127,159]).

(codepoint >= 0 && codepoint <= 31) ||
(codepoint >= 127 && codepoint <= 159) {
    charsets[charset_count++] = "iso_control";
}

## The punctuation character set
################################

# A punctuation character is any character that has one of the
# punctuation categories in the Unicode character database (Pc, Pd,
# Ps, Pe, Pi, Pf, or Po.)

# Note that srfi-14 gives conflicting requirements!!  It claims that
# only the Unicode punctuation is necessary, but, explicitly calls out
# the soft hyphen character (U+00AD) as punctution.  Current versions
# of Unicode consider U+00AD to be a formatting character, not
# punctuation.

category ~ /P/ {
    charsets[charset_count++] = "punctuation";
}

## The symbol character set
###########################

# A symbol is any character that has one of the symbol categories in
# the Unicode character database (Sm, Sc, Sk, or So).

category ~ /S/ {
    charsets[charset_count++] = "symbol";
}

## The blank character set
##########################

# Blank chars are horizontal whitespace.  A blank character is either
#    * a character with the space separator category (Zs) in the
#      Unicode character database.
#    * U+0009 (9) Horizontal tabulation (\t control-I)

category ~ /Zs/ || codepoint == 9 {
    charsets[charset_count++] = "blank";
}

## The ascii character set
##########################

codepoint <= 127 {
    charsets[charset_count++] = "ascii";
}

## The designated character set
###############################

# Designated -- All characters except for the surrogates

category !~ /Cs/ {
    charsets[charset_count++] = "designated";
}

## Other character sets
#######################

# Note that the "letter_plus_digit" and "printing" character sets, which
# are unions of other character sets, are included in the patterns
# matching their constituent parts (i.e., the "letter_plus_digit"
# character set is included as part of the "letter" and "digit"
# patterns).
#
# Also, the "empty" character is computed by doing precisely nothing!

# Keeping track of state
########################

# Update the state for each charset.
{
    for (i = 0; i < charset_count; i++) {
        cs = charsets[i];
        if (state[cs, "start"] == -1) {
            state[cs, "start"] = codepoint;
            state[cs, "end"] = codepoint_end;
        } else if (state[cs, "end"] + 1 == codepoint) {
            state[cs, "end"] = codepoint_end;
        } else {
            count = state[cs, "count"];
            state[cs, "count"]++;
            state[cs, "ranges", count, 0] = state[cs, "start"];
            state[cs, "ranges", count, 1] = state[cs, "end"];
            state[cs, "start"] = codepoint;
            state[cs, "end"] = codepoint_end;
        }
    }
}

# Printing and error handling
#############################

END {
    # Normally, an exit statement runs all the 'END' blocks before
    # actually exiting.  We use the 'exit_status' variable to short
    # circuit the rest of the 'END' block by reissuing the exit
    # statement.
    if (exit_status != 0) {
        exit exit_status;
    }

    # Write a bit of a header.
    print("/* srfi-14.i.c -- standard SRFI-14 character set data */");
    print("");
    print("/* This file is #include'd by srfi-14.c.  */");
    print("");
    print("/* This file was generated from");
    print("   https://unicode.org/Public/UNIDATA/UnicodeData.txt");
    print("   with the unidata_to_charset.awk script.  */");
    print("");

    for (i = 0; i < all_charsets_count; i++) {
        cs = all_charsets[i];

        # Extra logic to ensure that the last range is included.
        if (state[cs, "start"] != -1) {
            count = state[cs, "count"];
            state[cs, "count"]++;
            state[cs, "ranges", count, 0] = state[cs, "start"];
            state[cs, "ranges", count, 1] = state[cs, "end"];
        }

        count = state[cs, "count"];

        print("static const scm_t_char_range cs_" cs "_ranges[] = {");
        for (j = 0; j < count; j++) {
            rstart = state[cs, "ranges", j, 0];
            rend = state[cs, "ranges", j, 1];
            if (j + 1 < count) {
                printf("  {0x%04x, 0x%04x},\n", rstart, rend);
            } else {
                printf("  {0x%04x, 0x%04x}\n", rstart, rend);
            }
        }
        print("};");
        print("");

        count = state[cs, "count"];
        printf("static const size_t cs_%s_len = %d;\n", cs, count);
        if (i + 1 < all_charsets_count) {
            print("");
        }
    }
}

# And we're done.
