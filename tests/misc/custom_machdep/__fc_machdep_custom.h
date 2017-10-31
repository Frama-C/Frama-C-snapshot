/* skeleton of a real custom machdep header.
   Note: the values provided here are merely for illustrative purposes
         and are not necessarily consistent between them. */
#ifndef __FC_MACHDEP
#define __FC_MACHDEP

#ifdef __FC_MACHDEP_CUSTOM

/* Constants required by the C standard */
#undef  __CHAR_UNSIGNED__
#define __WORDSIZE 32
#define __SIZEOF_SHORT 2
#define __SIZEOF_INT 3
#define __SIZEOF_LONG 4
#define __SIZEOF_LONGLONG 8
#define __CHAR_BIT 8
#define __PTRDIFF_T int
#define __SIZE_T unsigned int

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

#define __INT_MAX_T signed long long
#define __UINT_MAX_T unsigned long long

#define __FC_PATH_MAX 256
#define __FC_SIZE_MAX __FC_ULLONG_MAX

/* Optional constants */
#define __INT8_T signed char
#define __UINT8_T unsigned char
#define __INT16_T signed short
#define __UINT16_T unsigned short

#define __INTPTR_T signed long
#define __UINTPTR_T unsigned long
#define __INT32_T signed long
#define __UINT32_T unsigned long
#define __INT64_T signed long long
#define __UINT64_T unsigned long long

/* Required constants */
#define __INT_LEAST8_T signed char
#define __UINT_LEAST8_T unsigned char
#define __INT_LEAST16_T signed short
#define __UINT_LEAST16_T unsigned short
#define __INT_LEAST32_T signed long
#define __UINT_LEAST32_T unsigned long
#define __INT_LEAST64_T signed long long
#define __UINT_LEAST64_T unsigned long long

#define __INT_FAST8_T signed char
#define __UINT_FAST8_T unsigned char
#define __INT_FAST16_T signed int
#define __UINT_FAST16_T unsigned int
#define __INT_FAST32_T signed long
#define __UINT_FAST32_T unsigned long
#define __INT_FAST64_T signed long long
#define __UINT_FAST64_T unsigned long long


/* POSIX */
#define __SSIZE_T int
/* stdint.h */
#define __FC_PTRDIFF_MIN __FC_INT_MIN
#define __FC_PTRDIFF_MAX __FC_INT_MAX
#define __FC_INTMAX_MIN (-9223372036854775807LL -1LL)
#define __FC_INTMAX_MAX 9223372036854775807LL
#define __FC_UINTMAX_MAX 18446744073709551615ULL

#define __FC_EOF (-1)
#define __FC_FOPEN_MAX 20
#define __FC_RAND_MAX 32767
#define __WCHAR_T unsigned short

/* for stdarg.h */
#define __FC_VA_LIST_T char*

/* for time.h */
#define __FC_TIME_T long

/* for wchar.h */
#define __WINT_T unsigned int
#define __FC_WEOF (0xFFFFFFFFU)
#define __FC_WINT_MIN 0
#define __FC_WINT_MAX __FC_UINT_MAX

#else
  error "I'm supposed to be called with __FC_MACHDEP_CUSTOM macro defined"
#endif
#endif
