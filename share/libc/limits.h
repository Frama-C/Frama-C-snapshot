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

/* ISO C: 7.10 and 5.2.4.2.1 */
#ifndef __FC_LIMITS
#define __FC_LIMITS

#include "__fc_machdep.h"

/* Number of bits in a `char'.	*/
#define CHAR_BIT __CHAR_BIT


/* Minimum and maximum values a `signed char' can hold.  */
#  define SCHAR_MIN	__smin(1,)
#  define SCHAR_MAX	__smax(1,)

/* Maximum value an `unsigned char' can hold.  (Minimum is 0.)  */
#  define UCHAR_MAX	__umax(unsigned char)

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
#  define SHRT_MIN	__smin(__SIZEOF_SHORT,)
#  define SHRT_MAX	__smax(__SIZEOF_SHORT,)

/* Maximum value an `unsigned short int' can hold.  (Minimum is 0.)  */
#  define USHRT_MAX	__umax(unsigned short)

/* Minimum and maximum values a `signed int' can hold.  */
#  define INT_MIN	__smin(__SIZEOF_INT,)
#  define INT_MAX	__smax(__SIZEOF_INT,)

/* Maximum value an `unsigned int' can hold.  (Minimum is 0.)  */
#  define UINT_MAX	__umax(unsigned int)

/* Minimum and maximum values a `signed long int' can hold.  */
#  define LONG_MAX	__smax(__SIZEOF_LONG,L)
#  define LONG_MIN	__smin(__SIZEOF_LONG,L)

/* Maximum value an `unsigned long int' can hold.  (Minimum is 0.)  */
#define ULONG_MAX	__umax(unsigned long)

/* Minimum and maximum values a `signed long long int' can hold.  */
#   define LLONG_MAX	__smax(__SIZEOF_LONGLONG,LL)
#   define LLONG_MIN	__smin(__SIZEOF_LONGLONG,LL)

/* Maximum value an `unsigned long long int' can hold.  (Minimum is 0.)  */
#   define ULLONG_MAX	__umax(unsigned long long)

#endif

