/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2008                                               */
/*    INRIA (Institut National de Recherche en Informatique et en         */
/*           Automatique)                                                 */
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
/**************************************************************************/

/* $Id: limits.h,v 1.3 2008/10/28 16:16:11 uid570 Exp $ */

#ifndef _LIMITS_H_
#define _LIMITS_H_

/* Number of bits in a `char'.	*/
#define CHAR_BIT	8

/* Minimum and maximum values a `signed char' can hold.  */
#define SCHAR_MIN	(-128)
#define SCHAR_MAX	127

/* Maximum value an `unsigned char' can hold.  (Minimum is 0.)  */
#define UCHAR_MAX	255

/* Minimum and maximum values a `char' can hold.  */
#define CHAR_MIN	0
#define CHAR_MAX	UCHAR_MAX

/* Minimum and maximum values a `signed short int' can hold.  */
#define SHRT_MIN	(-32768)
#define SHRT_MAX	32767

/* Maximum value an `unsigned short int' can hold.  (Minimum is 0.)  */
#define USHRT_MAX	65535

/* Minimum and maximum values a `signed int' can hold.  */
#define INT_MIN	        (-INT_MAX - 1)
#define INT_MAX	        2147483647

/* Maximum value an `unsigned int' can hold.  (Minimum is 0.)  */
#define UINT_MAX	4294967295U

/* Minimum and maximum values a `signed long int' can hold.  */
#define LONG_MAX	2147483647L
#define LONG_MIN	(-LONG_MAX - 1L)

/* Maximum value an `unsigned long int' can hold.  (Minimum is 0.)  */
#define ULONG_MAX	4294967295UL

/* Minimum and maximum values a `signed long long int' can hold.  */
#define LLONG_MAX	9223372036854775807LL
#define LLONG_MIN	(-LLONG_MAX - 1LL)

/* Maximum value an `unsigned long long int' can hold.  (Minimum is 0.)  */
#define ULLONG_MAX	18446744073709551615ULL

#endif /* _LIMITS_H_ */
