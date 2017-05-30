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

#include "ctype.h"
#include "__fc_builtin.h"

// This file assumes a simple, C-like locale, with no extra characters
// or special cases.

#define	ISDIGIT(_c) \
	((_c) >= '0' && (_c) <= '9')

#define	ISXDIGIT(_c) \
	(ISDIGIT(_c) || \
	((_c) >= 'a' && (_c) <= 'f') || \
	((_c) >= 'A' && (_c) <= 'F'))

// if locale = "C"
#define	ISLOWER(_c) \
	((_c) >= 'a' && (_c) <= 'z')

// if locale = "C"
#define	ISUPPER(_c) \
	((_c) >= 'A' && (_c) <= 'Z')

#define	ISALPHA(_c) \
	(ISUPPER(_c) || \
	ISLOWER(_c))

#define	ISALNUM(_c) \
	(ISALPHA(_c) || \
	ISDIGIT(_c))

// if locale = "C"
#define	ISSPACE(_c) \
	((_c) == ' ' || \
	(_c) == '\f' || \
	(_c) == '\n' || \
	(_c) == '\r' || \
	(_c) == '\t' || \
	(_c) == '\v' )

// if locale = "C"
#define	ISBLANK(_c) \
	((_c) == ' ' || \
	 (_c) == '\t')

int isalnum(int c) {
  return (ISALNUM(c));
}

int isalpha(int c){
  return (ISALPHA(c));
}

int isblank(int c){
  return (ISBLANK(c)||ISSPACE(c));
}

int iscntrl(int c) {
  return (Frama_C_nondet(0,1));
}

int isdigit(int c) {
  return (ISDIGIT(c));
}

int isgraph(int c) {
  return (Frama_C_nondet(0,1));
}

int islower(int c) {
  return (ISLOWER(c));
}

int isprint(int c) {
  return (Frama_C_nondet(0,1));
}

int ispunct(int c) {
  return (Frama_C_nondet(0,1));
}

int isspace(int c) {
  return (ISSPACE(c));
}

int isupper(int c) {
  return (ISUPPER(c));
}

int isxdigit(int c) {
  return (ISXDIGIT(c));
}

int tolower(int c) {
  if ((c >= 'A') && (c <= 'Z'))
    return c + 0x20;
  return c;
}

int toupper (int c)
{
  if ((c >= 'a') && (c <= 'z'))
    return c - 0x20;
  return c;
}
