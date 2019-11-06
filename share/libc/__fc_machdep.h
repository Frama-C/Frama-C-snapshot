/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2019                                               */
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

#if defined(__FC_MACHDEP_X86_32) || defined(__FC_MACHDEP_GCC_X86_32)
#define __FC_FORCE_INCLUDE_MACHDEP__
#include "__fc_machdep_linux_shared.h"
#ifdef __FC_MACHDEP_GCC_X86_32
#include "__fc_gcc_builtins.h"
#endif
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
#define __PTRDIFF_T int
#define __SIZE_T unsigned int
#define __FC_INT_MIN (-2147483647 - 1)
#define __FC_INT_MAX 2147483647
#define __FC_UINT_MAX 4294967295U
#define __FC_LONG_MIN (-2147483647L - 1L)
#define __FC_LONG_MAX 2147483647L
#define __FC_ULONG_MAX 4294967295UL
#define __FC_SIZE_MAX __FC_UINT_MAX

/* Optional */
#define __INTPTR_T signed int
#define __FC_INTPTR_MIN __FC_INT_MIN
#define __FC_INTPTR_MAX __FC_INT_MAX

#define __UINTPTR_T unsigned int
#define __FC_UINTPTR_MAX __FC_UINT_MAX

#define __INT32_T signed int
#define __UINT32_T unsigned int
#define __INT64_T signed long long
#define __UINT64_T unsigned long long

/* prefixes for inttypes */
#define __PRI32_PREFIX ""
#define __PRI64_PREFIX "ll"
#define __PRIPTR_PREFIX ""

/* Required */
#define __INT_LEAST32_T signed int
#define __INT_LEAST32_MIN __FC_INT_MIN
#define __INT_LEAST32_MAX __FC_INT_MAX

#define __UINT_LEAST32_T unsigned int
#define __UINT_LEAST32_MAX __FC_UINT_MAX

#define __INT_LEAST64_T signed long long
#define __INT_LEAST64_MIN __FC_LLONG_MIN
#define __INT_LEAST64_MAX __FC_LLONG_MAX

#define __UINT_LEAST64_T unsigned long long
#define __UINT_LEAST64_MAX __FC_ULLONG_MAX

#define __INT_FAST32_T signed int
#define __INT_FAST32_MIN __FC_INT_MIN
#define __INT_FAST32_MAX __FC_INT_MAX

#define __UINT_FAST32_T unsigned int
#define __UINT_FAST32_MAX __FC_UINT_MAX

#define __INT_FAST64_T signed long long
#define __INT_FAST64_MIN __FC_LLONG_MIN
#define __INT_FAST64_MAX __FC_LLONG_MAX

#define __UINT_FAST64_T unsigned long long
#define __UINT_FAST64_MAX __FC_ULLONG_MAX

/* POSIX */
#define __SSIZE_T int
#define __SSIZE_MAX __FC_INT_MAX
/* stdio.h */
#ifndef __FC_L_tmpnam
#define __FC_L_tmpnam 1024
#endif
/* stdint.h */
#define __FC_PTRDIFF_MIN __FC_INT_MIN
#define __FC_PTRDIFF_MAX __FC_INT_MAX
/* wchar.h */
#define __WINT_T unsigned int
#define __FC_WEOF (0xFFFFFFFFU)
#define __FC_WINT_MIN 0
#define __FC_WINT_MAX __FC_UINT_MAX

// End of X86_32 || GCC_X86_32
#else
#if defined(__FC_MACHDEP_X86_64) || defined(__FC_MACHDEP_GCC_X86_64)
#define __FC_FORCE_INCLUDE_MACHDEP__
#include "__fc_machdep_linux_shared.h"
#ifdef __FC_MACHDEP_GCC_X86_64
#include "__fc_gcc_builtins.h"
#endif
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
#define __PTRDIFF_T long
#define __SIZE_T unsigned long
#define __FC_INT_MIN (-2147483647 - 1)
#define __FC_INT_MAX 2147483647
#define __FC_UINT_MAX 4294967295U
#define __FC_LONG_MIN (-9223372036854775807L -1L)
#define __FC_LONG_MAX 9223372036854775807L
#define __FC_ULONG_MAX 18446744073709551615UL
#define __FC_SIZE_MAX __FC_ULONG_MAX

/* Optional */
#define __INTPTR_T signed long
#define __FC_INTPTR_MIN __FC_LONG_MIN
#define __FC_INTPTR_MAX __FC_LONG_MAX

#define __UINTPTR_T unsigned long
#define __FC_UINTPTR_MAX __FC_ULONG_MAX

#define __INT32_T signed int
#define __INT32_MIN __FC_INT_MIN
#define __INT32_MAX __FC_INT_MAX

#define __UINT32_T unsigned int
#define __UINT32_MAX __FC_UINT_MAX

#define __INT64_T signed long
#define __INT64_MIN __FC_LONG_MIN
#define __INT64_MAX __FC_LONG_MAX

#define __UINT64_T unsigned long
#define __UINT64_MAX __FC_ULONG_MAX

/* prefixes for inttypes */
#define __PRI32_PREFIX ""
#define __PRI64_PREFIX "l"
#define __PRIPTR_PREFIX "l"

/* Required */
#define __INT_LEAST32_T signed int
#define __INT_LEAST32_MIN __FC_INT_MIN
#define __INT_LEAST32_MAX __FC_INT_MAX

#define __UINT_LEAST32_T unsigned int
#define __UINT_LEAST32_MAX __FC_UINT_MAX

#define __INT_LEAST64_T signed long
#define __INT_LEAST64_MIN __FC_LONG_MIN
#define __INT_LEAST64_MAX __FC_LONG_MAX

#define __UINT_LEAST64_T unsigned long
#define __UINT_LEAST64_MAX __FC_ULONG_MAX

#define __INT_FAST32_T signed int
#define __INT_FAST32_MIN __FC_INT_MIN
#define __INT_FAST32_MAX __FC_INT_MAX

#define __UINT_FAST32_T unsigned int
#define __UINT_FAST32_MAX __FC_UINT_MAX

#define __INT_FAST64_T signed long
#define __INT_FAST64_MIN __FC_LONG_MIN
#define __INT_FAST64_MAX __FC_LONG_MAX

#define __UINT_FAST64_T unsigned long
#define __UINT_FAST64_MAX __FC_ULONG_MAX

/* POSIX */
#define __SSIZE_T signed long
#define __SSIZE_MAX __FC_LONG_MAX
/* stdio.h */
#ifndef __FC_L_tmpnam
#define __FC_L_tmpnam 1024
#endif
/* stdint.h */
#define __FC_PTRDIFF_MIN __FC_LONG_MIN
#define __FC_PTRDIFF_MAX __FC_LONG_MAX
/* wchar.h */
#define __WINT_T unsigned int
#define __FC_WEOF (0xFFFFFFFFU)
#define __FC_WINT_MIN 0
#define __FC_WINT_MAX __FC_UINT_MAX

// End of X86_64 || GCC_X86_64
#else
#if defined(__FC_MACHDEP_X86_16) || defined(__FC_MACHDEP_GCC_X86_16)
#define __FC_FORCE_INCLUDE_MACHDEP__
#include "__fc_machdep_linux_shared.h"
#ifdef __FC_MACHDEP_GCC_X86_16
#include "__fc_gcc_builtins.h"
#endif
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
#define __PTRDIFF_T long
#define __SIZE_T unsigned int
#define __FC_INT_MIN (-32768)
#define __FC_INT_MAX 32767
#define __FC_UINT_MAX 65535U
#define __FC_LONG_MIN (-2147483647L -1L)
#define __FC_LONG_MAX 2147483647L
#define __FC_ULONG_MAX 4294967295UL
#define __FC_SIZE_MAX 65535U


/* Optional */
#define __INTPTR_T signed long
#define __FC_INTPTR_MIN __FC_LONG_MIN
#define __FC_INTPTR_MAX __FC_LONG_MAX

#define __UINTPTR_T unsigned long
#define __FC_UINTPTR_MAX __FC_ULONG_MAX

#define __INT32_T signed long
#define __INT32_MIN __FC_LONG_MIN
#define __INT32_MAX __FC_LONG_MAX

#define __INT64_T signed long long
#define __INT64_MIN __FC_LLONG_MIN
#define __INT64_MAX __FC_LLONG_MAX

#define __UINT32_T unsigned long
#define __UINT32_MAX __FC_ULONG_MAX

#define __UINT64_T unsigned long long
#define __UINT64_MAX __FC_ULLONG_MAX

/* inttypes */

#define __PRI32_PREFIX "l"
#define __PRI64_PREFIX "ll"
#define __PRIPTR_PREFIX "l"

/* Required */
#define __INT_LEAST32_T signed long
#define __INT_LEAST32_MIN __FC_LONG_MIN
#define __INT_LEAST32_MAX __FC_LONG_MAX

#define __UINT_LEAST32_T unsigned long
#define __UINT_LEAST32_MAX __FC_ULONG_MAX

#define __INT_LEAST64_T signed long long
#define __INT_LEAST64_MIN __FC_LLONG_MIN
#define __INT_LEAST64_MAX __FC_LLONG_MAX

#define __UINT_LEAST64_T unsigned long long
#define __UINT_LEAST64_MAX __FC_ULLONG_MAX

#define __INT_FAST32_T signed long
#define __INT_FAST32_MIN __FC_LONG_MIN
#define __INT_FAST32_MAX __FC_LONG_MAX

#define __UINT_FAST32_T unsigned long
#define __UINT_FAST32_MAX __FC_ULONG_MAX

#define __INT_FAST64_T signed long long
#define __INT_FAST64_MIN __FC_LLONG_MIN
#define __INT_FAST64_MAX __FC_LLONG_MAX

#define __UINT_FAST64_T unsigned long long
#define __UINT_FAST64_MAX __FC_ULLONG_MAX

/* POSIX */
#define __SSIZE_T signed long
#define __SSIZE_MAX __FC_LONG_MAX
/* stdio.h */
#ifndef __FC_L_tmpnam
#define __FC_L_tmpnam 1024
#endif
/* stdint.h */
#define __FC_PTRDIFF_MIN __FC_LONG_MIN
#define __FC_PTRDIFF_MAX __FC_LONG_MAX

/* wchar.h */
#define __WINT_T unsigned long
#define __FC_WEOF (0xFFFFFFFFUL)
#define __FC_WINT_MIN 0
#define __FC_WINT_MAX __FC_ULONG_MAX

// End of X86_16 || GCC_X86_16
#else
#ifdef __FC_MACHDEP_PPC_32
#define __FC_FORCE_INCLUDE_MACHDEP__
#include "__fc_machdep_linux_shared.h"
#undef __FC_FORCE_INCLUDE_MACHDEP__
#define  __FC_BYTE_ORDER __BIG_ENDIAN
/* Required */
#undef  __CHAR_UNSIGNED__
#define __WORDSIZE 32
#define __SIZEOF_SHORT 2
#define __SIZEOF_INT 4
#define __SIZEOF_LONG 4
#define __SIZEOF_LONGLONG 8
#define __CHAR_BIT 8
#define __PTRDIFF_T int
#define __SIZE_T unsigned int
#define __FC_INT_MIN (-2147483647 - 1)
#define __FC_INT_MAX 2147483647
#define __FC_UINT_MAX 4294967295U
#define __FC_LONG_MIN (-2147483647L -1L)
#define __FC_LONG_MAX 2147483647L
#define __FC_ULONG_MAX 4294967295UL
#define __FC_SIZE_MAX __FC_UINT_MAX

/* Optional */
#define __INTPTR_T signed int
#define __FC_INTPTR_MIN __FC_LONG_MIN
#define __FC_INTPTR_MAX __FC_LONG_MAX

#define __UINTPTR_T unsigned int
#define __FC_UINTPTR_MAX __FC_ULONG_MAX

#define __INT32_T signed int
#define __INT32_MIN __FC_INT_MIN
#define __INT32_MAX __FC_INT_MAX

#define __UINT32_T unsigned int
#define __UINT32_MAX __FC_UINT_MAX

#define __INT64_T signed long long
#define __INT64_MIN __FC_LLONG_MIN
#define __INT64_MAX __FC_LLONG_MAX

#define __UINT64_T unsigned long long
#define __UINT64_MAX __FC_ULLONG_MAX

/* inttypes */

#define __PRI32_PREFIX ""
#define __PRI64_PREFIX "ll"
#define __PRIPTR_PREFIX ""

/* Required */
#define __INT_LEAST32_T signed int
#define __INT_LEAST32_MIN __FC_INT_MIN
#define __INT_LEAST32_MAX __FC_INT_MAX

#define __UINT_LEAST32_T unsigned int
#define __UINT_LEAST32_MAX __FC_UINT_MAX

#define __INT_LEAST64_T signed long long
#define __INT_LEAST64_MIN __FC_LLONG_MIN
#define __INT_LEAST64_MAX __FC_LLONG_MAX

#define __UINT_LEAST64_T unsigned long long
#define __UINT_LEAST64_MAX __FC_ULLONG_MAX

#define __INT_FAST32_T signed int
#define __INT_FAST32_MIN __FC_INT_MIN
#define __INT_FAST32_MAX __FC_INT_MAX

#define __UINT_FAST32_T unsigned int
#define __UINT_FAST32_MAX __FC_UINT_MAX

#define __INT_FAST64_T signed long long
#define __INT_FAST64_MIN __FC_LLONG_MIN
#define __INT_FAST64_MAX __FC_LLONG_MAX

#define __UINT_FAST64_T unsigned long long
#define __UINT_FAST64_MAX __FC_ULLONG_MAX

/* POSIX */
#define __SSIZE_T int
#define __SSIZE_MAX __FC_INT_MAX
/* stdio.h */
#ifndef __FC_L_tmpnam
#define __FC_L_tmpnam 1024
#endif
/* stdint.h */
#define __FC_PTRDIFF_MIN __FC_INT_MIN
#define __FC_PTRDIFF_MAX __FC_INT_MAX
#define __FC_INTMAX_MIN (-9223372036854775807LL -1LL)
#define __FC_INTMAX_MAX 9223372036854775807LL
#define __FC_UINTMAX_MAX 18446744073709551615ULL
/* time.h */
#define __FC_TIME_T long
/* wchar.h */
#define __WINT_T unsigned int
#define __FC_WEOF (0xFFFFFFFFU)
#define __FC_WINT_MIN 0
#define __FC_WINT_MAX __FC_UINT_MAX

// End of PPC_32
#else
#ifdef __FC_MACHDEP_MSVC_X86_64
#define  __FC_BYTE_ORDER __LITTLE_ENDIAN

// This machdep does NOT conform itself to POSIX.1-2008

/* Required */
#undef  __CHAR_UNSIGNED__
#define __WORDSIZE 64
#define __SIZEOF_SHORT 2
#define __SIZEOF_INT 4
#define __SIZEOF_LONG 4
#define __SIZEOF_LONGLONG 8
#define __CHAR_BIT 8
#define __PTRDIFF_T long long
#define __SIZE_T unsigned long long

#define __FC_EOF (-1)
#define __FC_FOPEN_MAX 20
#define __FC_RAND_MAX 32767
#define __WCHAR_T unsigned short

/* min and max values as specified in limits.h */
#define __FC_SCHAR_MIN (-128)
#define __FC_SCHAR_MAX 127
#define __FC_UCHAR_MAX 255
#define __FC_SHRT_MIN (-32768)
#define __FC_SHRT_MAX 32767
#define __FC_USHRT_MAX 65535
#define __FC_INT_MIN (-2147483647 - 1)
#define __FC_INT_MAX 2147483647
#define __FC_UINT_MAX 4294967295U
#define __FC_LONG_MIN (-2147483647L -1L)
#define __FC_LONG_MAX 2147483647L
#define __FC_ULONG_MAX 4294967295UL
#define __FC_LLONG_MIN (-9223372036854775807LL -1LL)
#define __FC_LLONG_MAX 9223372036854775807LL
#define __FC_ULLONG_MAX 18446744073709551615ULL
#define __FC_PATH_MAX 256
#define __FC_SIZE_MAX __FC_ULLONG_MAX
// Note: SSIZE_T/SSIZE_MAX are not defined in this machdep!
/* Note: MSVC does not define this constant, but because it is used in an ACSL
   specification, it is safer to define it anyway. */
#define __FC_HOST_NAME_MAX 255
#define __FC_TTY_NAME_MAX 32

/* Optional */
#define __INT8_T signed char
#define __INT8_MIN __FC_SCHAR_MIN
#define __INT8_MAX __FC_SCHAR_MAX

#define __UINT8_T unsigned char
#define __UINT8_MIN __FC_UCHAR_MAX

#define __INT16_T signed short
#define __INT16_MIN __FC_SHRT_MIN
#define __INT16_MAX __FC_SHRT_MAX

#define __UINT16_T unsigned short
#define __UINT16_MAX __FC_USHRT_MAX

#define __INTPTR_T signed long long
#define __FC_INTPTR_MIN __FC_LLONG_MIN
#define __FC_INTPTR_MAX __FC_LLONG_MAX

#define __UINTPTR_T unsigned long long
#define __FC_UINTPTR_MAX __FC_ULLONG_MAX

#define __INT32_T signed int
#define __INT32_MIN __FC_INT_MIN
#define __INT32_MAX __FC_INT_MAX

#define __UINT32_T unsigned int
#define __UINT32_MAX __FC_UINT_MAX

#define __INT64_T signed long long
#define __INT64_MIN __FC_LLONG_MIN
#define __INT64_MAX __FC_LLONG_MAX

#define __UINT64_T unsigned long long
#define __UINT64_MAX __FC_ULLONG_MAX

/* inttypes */

#define __PRI8_PREFIX "hh"
#define __PRI16_PREFIX "h"
#define __PRIFAST16_PREFIX ""
#define __PRI32_PREFIX ""
#define __PRI64_PREFIX "ll"
#define __PRIPTR_PREFIX "ll"
#define __PRIMAX_PREFIX "ll"

/* Required */
#define __INT_LEAST8_T signed char
#define __INT_LEAST8_MIN __FC_SCHAR_MIN
#define __INT_LEAST8_MAX __FC_SCHAR_MAX

#define __UINT_LEAST8_T unsigned char
#define __UINT_LEAST8_MAX __FC_UCHAR_MAX

#define __INT_LEAST16_T signed short
#define __INT_LEAST16_MIN __FC_SHRT_MIN
#define __INT_LEAST16_MAX __FC_SHRT_MAX

#define __UINT_LEAST16_T unsigned short
#define __UINT_LEAST16_MAX __FC_USHRT_MAX

#define __INT_LEAST32_T signed int
#define __INT_LEAST32_MIN __FC_INT_MIN
#define __INT_LEAST32_MAX __FC_INT_MAX

#define __UINT_LEAST32_T unsigned int
#define __UINT_LEAST32_MAX __FC_UINT_MAX

#define __INT_LEAST64_T signed long long
#define __INT_LEAST64_MIN __FC_LLONG_MIN
#define __INT_LEAST64_MAX __FC_LLONG_MAX

#define __UINT_LEAST64_T unsigned long long
#define __UINT_LEAST64_MAX __FC_ULLONG_MAX

#define __INT_FAST8_T signed char
#define __INT_FAST8_MIN __FC_SCHAR_MIN
#define __INT_FAST8_MAX __FC_SCHAR_MAX

#define __UINT_FAST8_T unsigned char
#define __UINT_FAST8_MAX __FC_UCHAR_MAX

#define __INT_FAST16_T signed int
#define __INT_FAST16_MIN __FC_INT_MIN
#define __INT_FAST16_MAX __FC_INT_MAX

#define __UINT_FAST16_T unsigned int
#define __UINT_FAST16_MAX __FC_UINT_MAX

#define __INT_FAST32_T signed int
#define __INT_FAST32_MIN __FC_INT_MIN
#define __INT_FAST32_MAX __FC_INT_MAX

#define __UINT_FAST32_T unsigned int
#define __UINT_FAST32_MAX __FC_UINT_MAX

#define __INT_FAST64_T signed long long
#define __INT_FAST64_MIN __FC_LLONG_MIN
#define __INT_FAST64_MAX __FC_LLONG_MAX

#define __UINT_FAST64_T unsigned long long
#define __UINT_FAST64_MAX __FC_ULLONG_MAX

/* Required */
#define __INT_MAX_T signed long long
#define __UINT_MAX_T unsigned long long

/* POSIX */
#define __SSIZE_T signed long long
/* stdio.h */
#ifndef __FC_L_tmpnam
#define __FC_L_tmpnam 20
#endif
/* stdint.h */
#define __FC_WCHAR_MIN 0
#define __FC_WCHAR_MAX __FC_USHRT_MAX
#define __FC_PTRDIFF_MIN __FC_LLONG_MIN
#define __FC_PTRDIFF_MAX __FC_LLONG_MAX
#define __FC_INTMAX_MIN (-9223372036854775807LL -1LL)
#define __FC_INTMAX_MAX 9223372036854775807LL
#define __FC_UINTMAX_MAX 18446744073709551615ULL
/* time.h */
#define __FC_TIME_T __int64

/* for stdarg.h */
#define __FC_VA_LIST_T char*

/* wchar.h */
// note: wint_t should contain all values of wchar_t plus WEOF; but this version
// of MSVC does not necessarily respect the standard
#define __WINT_T unsigned short
#define __FC_WEOF (0xFFFFU)
#define __FC_WINT_MIN 0
#define __FC_WINT_MAX __FC_USHRT_MAX

/* The following macros are defined to correspond to the version of MSVC used
   during the definition of some MSVC-specific features: Visual Studio 2010.
   They also help detecting, in some tests, whether we are in MSVC mode. */
#define _MSC_FULL_VER 160040219
#define _MSC_VER 1600

// MSVC-specific definitions; necessary when parsing MSVC libraries using
// non-MSVC preprocessors and compilers
#undef __ptr64
#define __ptr64
#undef __ptr32
#define __ptr32
#undef __unaligned
#define __unaligned
#undef __cdecl
#define __cdecl
#undef __possibly_notnullterminated
#define __possibly_notnullterminated
#ifndef errno_t
# define errno_t int
# define _ERRNO_T_DEFINED
#endif
#ifndef _WIN64
# define _WIN64 1
#endif
#ifndef _AMD64_
# define _AMD64_ 1
#endif
#ifndef _M_AMD64
# define _M_AMD64 1
#endif
#ifndef _M_X64
# define _M_X64 1
#endif
// End of MSVC_X86_64
#else
#error Must define __FC_MACHDEP_<M>, where <M> is one of the            \
  following: X86_32, X86_64, X86_16, GCC_X86_32, GCC_X86_64,            \
  GCC_X86_16, PPC_32, MSVC_X86_64.                                      \
  If you are using a custom machdep, you must include your machdep      \
  header file defining __FC_MACHDEP to avoid inclusion of this file.
#endif
#endif
#endif
#endif
#endif
#endif
