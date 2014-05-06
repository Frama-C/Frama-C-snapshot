/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2014                                               */
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

/* ISO C: 7.10 and 5.2.4.2.1 */
#ifndef __FC_LIMITS
#define __FC_LIMITS

#include "__fc_machdep.h"

/* Number of bits in a `char'.	*/
#define CHAR_BIT __CHAR_BIT


/* Minimum and maximum values a `signed char' can hold.  */
#  define SCHAR_MIN	__FC_SCHAR_MIN
#  define SCHAR_MAX	__FC_SCHAR_MAX

/* Maximum value an `unsigned char' can hold.  (Minimum is 0.)  */
#  define UCHAR_MAX	__FC_UCHAR_MAX

/* Minimum and maximum values a `char' can hold.  */
#  ifdef __CHAR_UNSIGNED__
#   define CHAR_MIN	0
#   define CHAR_MAX	UCHAR_MAX
#  else
#   define CHAR_MIN	SCHAR_MIN
#   define CHAR_MAX	SCHAR_MAX
#  endif

#define MB_LEN_MAX 16

/* Minimum and maximum values a `signed short int' can hold.  */
#  define SHRT_MIN	__FC_SHRT_MIN
#  define SHRT_MAX	__FC_SHRT_MAX

/* Maximum value an `unsigned short int' can hold.  (Minimum is 0.)  */
#  define USHRT_MAX     __FC_USHRT_MAX

/* Minimum and maximum values a `signed int' can hold.  */
#  define INT_MIN	__FC_INT_MIN
#  define INT_MAX	__FC_INT_MAX

/* Maximum value an `unsigned int' can hold.  (Minimum is 0.)  */
#  define UINT_MAX	__FC_UINT_MAX

/* Minimum and maximum values a `signed long int' can hold.  */
#  define LONG_MAX	__FC_LONG_MAX
#  define LONG_MIN	__FC_LONG_MIN

/* Maximum value an `unsigned long int' can hold.  (Minimum is 0.)  */
#define ULONG_MAX	__FC_ULONG_MAX

/* Minimum and maximum values a `signed long long int' can hold.  */
#   define LLONG_MAX	__FC_LLONG_MAX
#   define LLONG_MIN	__FC_LLONG_MIN

/* Maximum value an `unsigned long long int' can hold.  (Minimum is 0.)  */
#   define ULLONG_MAX	__FC_ULLONG_MAX

#endif

