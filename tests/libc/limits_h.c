/*run.config
  STDOPT: #"-machdep x86_16"
  STDOPT: #"-machdep x86_32"
  STDOPT: #"-machdep x86_64"
  STDOPT: #"-machdep gcc_x86_16"
  STDOPT: #"-machdep gcc_x86_32"
  STDOPT: #"-machdep gcc_x86_64"
  STDOPT: #"-machdep ppc_32"
  STDOPT: #"-machdep msvc_x86_64"
*/
#include <sys/types.h>
#include <stdint.h>
#include <limits.h>

void main() {
  /*@ assert LLONG_MIN <= LONG_MIN <= INT_MIN <= SHRT_MIN <= SCHAR_MIN <=
    CHAR_MIN <= 0; */
  /*@ assert 0 <= CHAR_MAX <= UCHAR_MAX <= SHRT_MAX <= USHRT_MAX; */
  /*@ assert SHRT_MAX <= INT_MAX <= LONG_MAX <= LLONG_MAX; */
  /*@ assert USHRT_MAX <= UINT_MAX <= ULONG_MAX <= ULLONG_MAX; */
  /*@ assert INT_MAX <= UINT_MAX; */
  /*@ assert LONG_MAX <= ULONG_MAX; */
  /*@ assert LLONG_MAX <= ULLONG_MAX; */
  /*@ assert SCHAR_MIN == -SCHAR_MAX -1; */
  /*@ assert SHRT_MIN == -SHRT_MAX -1; */
  /*@ assert INT_MIN == -INT_MAX -1; */
  /*@ assert LONG_MIN == -LONG_MAX -1; */
  /*@ assert LLONG_MIN == -LLONG_MAX -1; */
  /*@ assert UINT_LEAST8_MAX >= INT_LEAST8_MAX == -INT_LEAST8_MIN -1; */
  /*@ assert UINT_FAST8_MAX >= INT_FAST8_MAX == -INT_FAST8_MIN -1; */
  /*@ assert UINT_LEAST16_MAX >= INT_LEAST16_MAX == -INT_LEAST16_MIN -1; */
  /*@ assert UINT_FAST16_MAX >= INT_FAST16_MAX == -INT_FAST16_MIN -1; */
  /*@ assert UINT_LEAST32_MAX >= INT_LEAST32_MAX == -INT_LEAST32_MIN -1; */
  /*@ assert UINT_FAST32_MAX >= INT_FAST32_MAX == -INT_FAST32_MIN -1; */
  /*@ assert UINT_LEAST64_MAX >= INT_LEAST64_MAX == -INT_LEAST64_MIN -1; */
  /*@ assert UINT_FAST64_MAX >= INT_FAST64_MAX == -INT_FAST64_MIN -1; */
  /*@ assert INTMAX_MIN == -INTMAX_MAX -1; */
  /*@ assert WCHAR_MIN <= WCHAR_MAX; */
  size_t size_max = SIZE_MAX;
#ifdef __FC_POSIX_VERSION
  ssize_t ssize_max = SSIZE_MAX;
#endif
  intptr_t intptr_max = INTPTR_MAX;
  intptr_t intptr_min = INTPTR_MIN;
  uintptr_t uintptr_max = UINTPTR_MAX;
  uintmax_t uintmax_max = UINTMAX_MAX;
}
