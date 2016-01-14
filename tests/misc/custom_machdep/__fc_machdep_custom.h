/* skeleton of a real custom machdep header. */
#ifndef __FC_MACHDEP
#define __FC_MACHDEP

#ifdef __FC_MACHDEP_CUSTOM

#define __FC_MACHDEP
#define __FC_SCHAR_MIN (-128)
#define __FC_SCHAR_MAX 127
#define __FC_UCHAR_MAX 255
#define __FC_SHRT_MIN	(-32768)
#define __FC_SHRT_MAX	32767
#define __FC_USHRT_MAX	65535
#define __FC_INT_MIN (-INT_MAX - 1)
#define __FC_INT_MAX 8388607
#define __FC_UINT_MAX 16777216
#define __FC_LONG_MIN (-LONG_MAX -1L)
#define __FC_LONG_MAX 2147483647L
#define __FC_ULONG_MAX 4294967295UL
#define __FC_LLONG_MIN (-LLONG_MAX -1LL)
#define __FC_LLONG_MAX 9223372036854775807LL
#define __FC_ULLONG_MAX 18446744073709551615ULL
#define __FC_PATH_MAX 256

#else
  error "I'm supposed to be called with __FC_MACHDEP_CUSTOM macro defined"
#endif
#endif
