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

/* ISO C: 7.18 */
#ifndef __FC_STDINT
#define __FC_STDINT
#include "__fc_machdep.h"

/* ISO C: 7.18.1.1 */
#ifdef __INT8_T
typedef __INT8_T int8_t;
#endif
#ifdef __UINT8_T
typedef __UINT8_T uint8_t;
#endif
#ifdef __INT16_T
typedef __INT16_T int16_t;
#endif
#ifdef __UINT16_T
typedef __UINT16_T uint16_t;
#endif
#ifdef __INT32_T
typedef __INT32_T int32_t;
#endif
#ifdef __UINT32_T
typedef __UINT32_T uint32_t;
#endif
#ifdef __INT64_T
typedef __INT64_T int64_t;
#endif
#ifdef __UINT64_T
typedef __UINT64_T uint64_t;
#endif

/* ISO C: 7.18.1.2 */
typedef __INT_LEAST8_T int_least8_t;
typedef __UINT_LEAST8_T uint_least8_t;
typedef __INT_LEAST16_T int_least16_t;
typedef __UINT_LEAST16_T uint_least16_t;
typedef __INT_LEAST32_T int_least32_t;
typedef __UINT_LEAST32_T uint_least32_t;
typedef __INT_LEAST64_T int_least64_t;
typedef __UINT_LEAST64_T uint_least64_t;

/* ISO C: 7.18.1.3 */
typedef __INT_FAST8_T int_fast8_t;
typedef __UINT_FAST8_T uint_fast8_t;
typedef __INT_FAST16_T int_fast16_t;
typedef __UINT_FAST16_T uint_fast16_t;
typedef __INT_FAST32_T int_fast32_t;
typedef __UINT_FAST32_T uint_fast32_t;
typedef __INT_FAST64_T int_fast64_t;
typedef __UINT_FAST64_T uint_fast64_t;

/* ISO C: 7.18.1.4 */
#include "__fc_define_intptr_t.h"

#ifdef __UINTPTR_T
typedef __UINTPTR_T uintptr_t;
#endif

/* ISO C: 7.18.1.5 */
typedef __INT_MAX_T intmax_t;
typedef __UINT_MAX_T uintmax_t;

/* ISO C: 7.18.2.1 */
#define INT8_MIN __smin(8)
#define INT8_MAX __smax(8)
#define UINT8_MAX __umax(8)
#define INT16_MIN __smin(16)
#define INT16_MAX  __smax(16)
#define UINT16_MAX __umax(16)
#define INT32_MIN __smin(32)
#define INT32_MAX __smax(32)
#define UINT32_MAX __umax(32)
#define INT64_MIN __smin(64)
#define INT64_MAX __smax(64)
#define UINT64_MAX __umax(64)

/* ISO C: 7.18.2.3-5 : TODO */

/* ISO C: 7.18.3 : TODO */

/* ISO C: 7.18.4 : TODO */


#endif
