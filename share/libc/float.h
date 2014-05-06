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

/* ISO C: 7.7 */

#ifndef __FC_FLOAT
#define __FC_FLOAT

/* Note: Values for long double are only valid for x86 extended format.
   Some black magic will be needed if some other format (or none) is
   supposed to be provided. */

#define FLT_RADIX		2

#define FLT_MANT_DIG		24
#define DBL_MANT_DIG		53
#define LDBL_MANT_DIG		64

#define FLT_DIG			6
#define DBL_DIG			15
#define LDBL_DIG		18

#define FLT_MIN_EXP		-125
#define DBL_MIN_EXP		-1021
#define LDBL_MIN_EXP		-16381

#define FLT_MIN_10_EXP		-37
#define DBL_MIN_10_EXP		-307
#define LDBL_MIN_10_EXP		-4931

#define FLT_MAX_EXP		128
#define DBL_MAX_EXP		1024
#define LDBL_MAX_EXP		16384

#define FLT_MAX_10_EXP		38
#define DBL_MAX_10_EXP		308
#define LDBL_MAX_10_EXP		4932

#define FLT_MAX			0x1.fffffep+127
#define DBL_MAX			0x1.fffffffffffffp+1023
#define LDBL_MAX		0x1.fffffffffffffffep+16383

#define FLT_EPSILON		0x1p-23
#define DBL_EPSILON		0x1p-52
#define LDBL_EPSILON		0x1p-63

#define FLT_MIN			0x1p-126
#define DBL_MIN			0x1p-1022
#define LDBL_MIN		0x1p-16382

#define FLT_ROUNDS		-1
#define FLT_EVAL_METHOD		-1

#endif
