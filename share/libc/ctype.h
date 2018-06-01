/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2018                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifndef __FC_CTYPE
#define __FC_CTYPE
#include "features.h"
__PUSH_FC_STDLIB
#include "__fc_define_eof.h"

/* ISO C : 7.4.1 */

__BEGIN_DECLS

/* Functions which have no complete behaviors are either due to
   locale-dependent characters, or under-specification by the
   C standard. */

/* Note: most functions use '\result < 0 || \result > 0' instead of
   of '\result != 0' for better precision if there is enough slevel. */

/*@
  requires c_uchar_or_eof_or_EOF: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes c_alnum: 'A' <= c <= 'Z' || 'a' <= c <= 'z' || '0' <= c <= '9';
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c_non_alnum: c == EOF || 0 <= c <= 47 || 58 <= c <= 64 ||
            91 <= c <= 96 || 123 <= c <= 127;
    ensures zero_result: \result == 0;
  disjoint behaviors;
*/
extern int isalnum(int c);

/*@ 
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes c_alpha: 'A' <= c <= 'Z' || 'a' <= c <= 'z';
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c_non_alpha: c == EOF || 0 <= c <= 64 || 91 <= c <= 96 || 123 <= c <= 127;
    ensures zero_result: \result == 0;
  disjoint behaviors;
*/
extern int isalpha(int c);

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior match:
    assumes c_tab_or_space: c == ' ' || c == '\t';
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior no_match:
    assumes c_non_blank: c != ' ' && c != '\t';
    ensures zero_result: \result == 0;
  disjoint behaviors;
  complete behaviors;
*/
extern int isblank(int c);

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes c_control_char: 0 <= c <= 31 || c == 127;
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c_non_control_char: c == EOF || 32 <= c <= 126;
    ensures zero_result: \result == 0;
  disjoint behaviors;
*/
extern int iscntrl(int c);

/*@ requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior match:
    assumes c_digit: '0' <= c <= '9';
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior no_match:
    assumes c_non_digit: c < '0' || c > '9';
    ensures zero_result: \result == 0;
  disjoint behaviors;
  complete behaviors;
*/
extern int isdigit(int c);

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes c_graphical: 33 <= c <= 126;
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c_non_graphical: c == EOF || 0 <= c <= 32 || c == 127;
    ensures zero_result: \result == 0;
  disjoint behaviors;
*/
extern int isgraph(int c);

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes c_lower: 'a' <= c <= 'z';
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c_non_lower: c == EOF || 0 <= c < 'a' || 'z' < c < 127;
    ensures zero_result: \result == 0;
  disjoint behaviors;
*/
extern int islower(int c);

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes c_printable: 32 <= c <= 126;
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c_non_printable: c == EOF || 0 <= c <= 31 || c == 127;
    ensures zero_result: \result == 0;
  disjoint behaviors;
*/
extern int isprint(int c);

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes c_punct: 33 <= c <= 47 || 58 <= c <= 64 || 91 <= c <= 96 || 123 <= c <= 126;
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c_non_punct: c == EOF || 0 <= c <= 32 || 48 <= c <= 57 || 65 <= c <= 90 ||
            97 <= c <= 122 || c == 127;
    ensures zero_result: \result == 0;
  disjoint behaviors;
*/
extern int ispunct(int c);

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes c_space: 9 <= c <= 13 || c == ' ';
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c_non_space: c == EOF || 0 <= c <= 8 || 14 <= c < ' ' || ' ' < c <= 127;
    ensures zero_result: \result == 0;
  disjoint behaviors;
*/
extern int isspace(int c);

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes c_upper: 'A' <= c <= 'Z';
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c_non_upper: c == EOF || 0 <= c < 'A' || 'Z' < c <= 127;
    ensures zero_result: \result == 0;
  disjoint behaviors;
*/
extern int isupper(int c);

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior match:
    assumes c_hexa_digit: '0' <= c <= '9' || 'A' <= c <= 'F' || 'a' <= c <= 'f';
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior no_match:
    assumes c_non_hexa_digit: !('0' <= c <= '9' || 'A' <= c <= 'F' || 'a' <= c <= 'f');
    ensures zero_result: \result == 0;
  disjoint behaviors;
  complete behaviors;
*/
extern int isxdigit(int c);

/* ISO C : 7.4.2 */

/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  ensures result_uchar_of_eof: 0 <= \result <= 255 || \result == EOF;
  behavior definitely_changed:
    assumes c_ascii_upper: 'A' <= c <= 'Z';
    ensures result_ascii_lower: \result == c + 32;
  behavior definitely_not_changed:
    assumes c_ascii_but_non_upper: c == EOF || 0 <= c < 'A' || 'Z' < c <= 127;
    ensures result_unchanged: \result == c;
  disjoint behaviors;
*/
extern int tolower(int c);

/*@
  requires c_uchar_of_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  ensures result_uchar_of_eof: 0 <= \result <= 255 || \result == EOF;
  behavior definitely_changed:
    assumes c_ascii_lower: 'a' <= c <= 'z';
    ensures result_ascii_upper: \result == c - 32;
  behavior definitely_not_changed:
    assumes c_ascii_but_non_lower: c == EOF || 0 <= c < 'a' || 'z' < c <= 127;
    ensures result_unchanged: \result == c;
  disjoint behaviors;
*/
extern int toupper(int c);

/* POSIX */
/*@
  requires c_uchar_or_eof: 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior match:
    assumes c_ascii: 0 <= c <= 127;
    ensures nonzero_result: \result < 0 || \result > 0;
  behavior no_match:
    assumes c_non_ascii: !(0 <= c <= 127);
    ensures zero_result: \result == 0;
  disjoint behaviors;
  complete behaviors;
*/
extern int isascii(int c);

__END_DECLS

__POP_FC_STDLIB
#endif
