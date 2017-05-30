/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2017                                               */
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

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_not_match:
    assumes c == EOF || 0 <= c <= 47 || 58 <= c <= 64 ||
            91 <= c <= 96 || 123 <= c <= 127;
    ensures \result == 0;
  behavior definitely_match:
    assumes 'A' <= c <= 'Z' || 'a' <= c <= 'z' || '0' <= c <= '9';
    ensures \result < 0 || \result > 0;
  disjoint behaviors definitely_match, definitely_not_match;
*/
extern int isalnum(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_not_match:
    assumes c == EOF || 0 <= c <= 64 || 91 <= c <= 96 || 123 <= c <= 127;
    ensures \result == 0;
  behavior definitely_match:
    assumes 'A' <= c <= 'Z' || 'a' <= c <= 'z';
    ensures \result < 0 || \result > 0;
  disjoint behaviors definitely_match, definitely_not_match;
*/
extern int isalpha(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior match:
    assumes c == ' ' || c == '\t';
    ensures \result < 0 || \result > 0;
  behavior no_match:
    assumes c != ' ' && c != '\t';
    ensures \result == 0;
  disjoint behaviors match, no_match;
  complete behaviors match, no_match;
*/
extern int isblank(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes 0 <= c <= 31 || c == 127;
    ensures \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c == EOF || 32 <= c <= 126;
    ensures \result == 0;
  disjoint behaviors definitely_match, definitely_not_match;
*/
extern int iscntrl(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior match:
    assumes '0' <= c <= '9';
    ensures \result < 0 || \result > 0;
  behavior no_match:
    assumes c < '0' || c > '9';
    ensures \result == 0;
  disjoint behaviors match, no_match;
  complete behaviors match, no_match;
*/
extern int isdigit(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes 33 <= c <= 126;
    ensures \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c == EOF || 0 <= c <= 32 || c == 127;
    ensures \result == 0;
  disjoint behaviors definitely_match, definitely_not_match;
*/
extern int isgraph(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes 'a' <= c <= 'z';
    ensures \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c == EOF || 0 <= c < 'a' || 'z' < c < 127;
    ensures \result == 0;
  disjoint behaviors definitely_match, definitely_not_match;
*/
extern int islower(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes 32 <= c <= 126;
    ensures \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c == EOF || 0 <= c <= 31 || c == 127;
    ensures \result == 0;
  disjoint behaviors definitely_match, definitely_not_match;
*/
extern int isprint(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes 33 <= c <= 47 || 58 <= c <= 64 || 91 <= c <= 96 || 123 <= c <= 126;
    ensures \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c == EOF || 0 <= c <= 32 || 48 <= c <= 57 || 65 <= c <= 90 ||
            97 <= c <= 122 || c == 127;
    ensures \result == 0;
  disjoint behaviors definitely_match, definitely_not_match;
*/
extern int ispunct(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes 9 <= c <= 13 || c == ' ';
    ensures \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c == EOF || 0 <= c <= 8 || 14 <= c < ' ' || ' ' < c <= 127;
    ensures \result == 0;
  disjoint behaviors definitely_match, definitely_not_match;
*/
extern int isspace(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior definitely_match:
    assumes 'A' <= c <= 'Z';
    ensures \result < 0 || \result > 0;
  behavior definitely_not_match:
    assumes c == EOF || 0 <= c < 'A' || 'Z' < c <= 127;
    ensures \result == 0;
  disjoint behaviors definitely_match, definitely_not_match;
*/
extern int isupper(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior match:
    assumes '0' <= c <= '9' || 'A' <= c <= 'F' || 'a' <= c <= 'f';
    ensures \result < 0 || \result > 0;
  behavior no_match:
    assumes !('0' <= c <= '9' || 'A' <= c <= 'F' || 'a' <= c <= 'f');
    ensures \result == 0;
  disjoint behaviors match, no_match;
  complete behaviors match, no_match;
*/
extern int isxdigit(int c);

/* ISO C : 7.4.2 */

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  ensures 0 <= \result <= 255 || \result == EOF;
  behavior definitely_changed:
    assumes 'A' <= c <= 'Z';
    ensures \result == c + 32;
  behavior definitely_not_changed:
    assumes c == EOF || 0 <= c < 'A' || 'Z' < c <= 127;
    ensures \result == c;
  disjoint behaviors definitely_changed, definitely_not_changed;
*/
extern int tolower(int c);

/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  ensures 0 <= \result <= 255 || \result == EOF;
  behavior definitely_changed:
    assumes 'a' <= c <= 'z';
    ensures \result == c - 32;
  behavior definitely_not_changed:
    assumes c == EOF || 0 <= c < 'a' || 'z' < c <= 127;
    ensures \result == c;
  disjoint behaviors definitely_changed, definitely_not_changed;
*/
extern int toupper(int c);

/* POSIX */
/*@ requires 0 <= c <= 255 || c == EOF;
  assigns \result \from c;
  behavior match:
    assumes 0 <= c <= 127;
    ensures \result < 0 || \result > 0;
  behavior no_match:
    assumes !(0 <= c <= 127);
    ensures \result == 0;
  disjoint behaviors match, no_match;
  complete behaviors match, no_match;
*/
extern int isascii(int c);

__END_DECLS

__POP_FC_STDLIB
#endif
