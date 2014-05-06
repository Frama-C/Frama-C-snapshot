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

#ifndef __FC_MACHDEP
#define __FC_MACHDEP

#ifndef __FC_MACHDEP_X86_64
#ifndef __FC_MACHDEP_X86_16
#define __FC_MACHDEP_X86_32
#endif
#endif

#ifdef __FC_MACHDEP_X86_32
#define __FC_FORCE_INCLUDE_MACHDEP__
#include "__fc_machdep_linux_gcc_shared.h"
#undef __FC_FORCE_INCLUDE_MACHDEP__
#define  __FC_BYTE_ORDER __LITTLE_ENDIAN
/* Required */
#undef  __CHAR_UNSIGNED__
#define __WORDSIZE 32
#define __SIZEOF_SHORT 2
#define __SIZEOF_INT 4
#define __SIZEOF_LONG 4
#define __SIZEOF_LONGLONG 8
#define __CHAR_BIT 8
#define __SIZE_T unsigned int
#define __PTRDIFF_T int
#define __FC_LONG_MAX 2147483647L
#define __FC_ULONG_MAX 4294967295UL

/* Optional */
#define __INTPTR_T signed int
#define __UINTPTR_T unsigned int
#define __INT32_T signed int
#define __UINT32_T unsigned int
#define __INT64_T signed long long
#define __UINT64_T unsigned long long

/* Required */
#define __INT_LEAST32_T signed int
#define __UINT_LEAST32_T unsigned int
#define __INT_FAST32_T signed int
#define __UINT_FAST32_T unsigned int

/* POSIX */
#define __SSIZE_T int
/* stdint.h */
#define __FC_PTRDIFF_MIN __FC_INT_MIN
#define __FC_PTRDIFF_MAX __FC_INT_MAX

#else
#ifdef __FC_MACHDEP_X86_64
#define __FC_FORCE_INCLUDE_MACHDEP__
#include "__fc_machdep_linux_gcc_shared.h"
#undef __FC_FORCE_INCLUDE_MACHDEP__
#define  __FC_BYTE_ORDER __LITTLE_ENDIAN
/* Required */
#undef  __CHAR_UNSIGNED__
#define __WORDSIZE 64
#define __SIZEOF_SHORT 2
#define __SIZEOF_INT 4
#define __SIZEOF_LONG 8
#define __SIZEOF_LONGLONG 8
#define __CHAR_BIT 8
#define __SIZE_T unsigned long
#define __PTRDIFF_T long
#define __FC_LONG_MAX 9223372036854775807L
#define __FC_ULONG_MAX 18446744073709551615UL

/* Optional */
#define __INTPTR_T signed long
#define __UINTPTR_T unsigned long
#define __INT32_T signed int
#define __UINT32_T unsigned int
#define __INT64_T signed long long
#define __UINT64_T unsigned long long

/* Required */
#define __INT_LEAST32_T signed int
#define __UINT_LEAST32_T unsigned int
#define __INT_FAST32_T signed int
#define __UINT_FAST32_T unsigned int

/* POSIX */
#define __SSIZE_T signed long
/* stdint.h */
#define __FC_PTRDIFF_MIN __FC_LONG_MIN
#define __FC_PTRDIFF_MAX __FC_LONG_MAX
#else
#ifdef __FC_MACHDEP_X86_16
#define __FC_FORCE_INCLUDE_MACHDEP__
#include "__fc_machdep_linux_gcc_shared.h"
#undef __FC_FORCE_INCLUDE_MACHDEP__
#define  __FC_BYTE_ORDER __LITTLE_ENDIAN
/* Required */
#undef  __CHAR_UNSIGNED__
#define __WORDSIZE 16
#define __SIZEOF_SHORT 2
#define __SIZEOF_INT 2
#define __SIZEOF_LONG 4
#define __SIZEOF_LONGLONG 8
#define __CHAR_BIT 8
#define __SIZE_T unsigned long
#define __PTRDIFF_T long
#define __FC_LONG_MAX 2147483647L
#define __FC_ULONG_MAX 4294967295UL

/* Optional */
#define __INTPTR_T signed long
#define __UINTPTR_T unsigned long
#define __INT32_T signed long
#define __UINT32_T unsigned long
#define __INT64_T signed long long
#define __UINT64_T unsigned long long

/* Required */
#define __INT_LEAST32_T signed long
#define __UINT_LEAST32_T unsigned long
#define __INT_FAST32_T signed long
#define __UINT_FAST32_T unsigned long

/* POSIX */
#define __SSIZE_T signed long
/* stdint.h */
#define __FC_PTRDIFF_MIN __FC_LONG_MIN
#define __FC_PTRDIFF_MAX __FC_LONG_MAX
#else

#error Must define __FC_MACHDEP_X86_32 or __FC_MACHDEP_X86_64 or \
       __FC_MACHDEP_X86_16.
#endif
#endif
#endif
#endif
