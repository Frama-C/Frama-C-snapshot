// This test should compile with GCC and obtain the same results.
// The output format is similar to the one given by Value, so that a diff between
// both files should output identical lines (besides lots of extra lines).
// This test should also compile with MSVC, although due to the lack of
// __attribute__, results will be different.

#define _STR_(x) x
#define STR(x) _STR_(#x)

// Some versions of MSVC do not accept the '%z' modifier, but GCC emits warnings
// without it, so use this macro to print it in both compilers.
// Note that __GNUC__ is defined in our machdep MSVC, but not in the actual
// MSVC compiler itself.
#ifdef __GNUC__
// GCC uses %zu for size_t, and allows unicode
# define ZU "%zu"
# define IN "∈"
#else
// MSVC uses %u for size_t, and does not allow unicode
# define ZU "%u"
# define IN "IN"
// this include is necessary when testing on Visual C++
# include "stdafx.h"
#endif

// To avoid polluting Value output with printfs,
// add it conditionally (for testing with GCC/Clang/MSVC)
#ifdef __FRAMAC__
#define PRINTF(...)
#else
#include <stdio.h>
#define PRINTF printf
#endif
