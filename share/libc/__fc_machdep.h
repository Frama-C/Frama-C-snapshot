/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2011                                               */
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

#ifndef __FRAMA_C_MACHDEP
#define __FRAMA_C_MACHDEP

#define __FRAMA_C_MACHDEP_x86_32

#ifdef __FRAMA_C_MACHDEP_x86_32

/* Required */
#undef  __CHAR_UNSIGNED__
#define __WORDSIZE 32
#define __SIZEOF_SHORT 2
#define __SIZEOF_INT 4
#define __SIZEOF_LONG 4
#define __SIZEOF_LONGLONG 8
#define __CHAR_BIT 8
#define __SIZE_T unsigned int
#define __WCHAR_T int
#define __PTRDIFF_T int

/* Optional */
#define __INT8_T signed char
#define __UINT8_T unsigned char
#define __INT16_T signed short
#define __UINT16_T unsigned short
#define __INT32_T signed int
#define __UINT32_T unsigned int
#define __INT64_T signed long long
#define __UINT64_T unsigned long long

/* Required */
#define __INT_LEAST8_T signed char
#define __UINT_LEAST8_T unsigned char
#define __INT_LEAST16_T signed short
#define __UINT_LEAST16_T unsigned short
#define __INT_LEAST32_T signed int
#define __UINT_LEAST32_T unsigned int
#define __INT_LEAST64_T signed long long
#define __UINT_LEAST64_T unsigned long long

/* Required */
#define __INT_FAST8_T signed char
#define __UINT_FAST8_T unsigned char
#define __INT_FAST16_T signed short
#define __UINT_FAST16_T unsigned short
#define __INT_FAST32_T signed int
#define __UINT_FAST32_T unsigned int
#define __INT_FAST64_T signed long long
#define __UINT_FAST64_T unsigned long long

/* Optional */
#define __INTPTR_T signed int
#define __UINTPTR_T unsigned int

/* Required */
#define __INT_MAX_T signed long long
#define __UINT_MAX_T unsigned long long

/* POSIX */
#define __SSIZE_T int

#else
#error Must define "__FRAMA_C_MACHDEP" to x86_32
#endif

#endif

#define __umax(typ) ((typ)(-1))
#define __smin(oct,TYP) (2*(-(1##TYP << (oct*__CHAR_BIT - 2))))
#define __smax(oct,TYP) ((1##TYP << (oct*__CHAR_BIT - 2))-1+(1##TYP << (oct*__CHAR_BIT - 2)))
