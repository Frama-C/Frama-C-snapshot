/* run.config
   STDOPT: +"-cpp-extra-args=-DTEST_IMPLEMENTATION=1" +"-slevel 1000"
   STDOPT: +"-slevel 1000"
   COMMENT: slevel is used to ensure all loops are unrolled (including in the
   COMMENT: implementation). 'goto exit' avoids recomputing split branches.
*/

#include <wchar.h>

// check that the following types/macros are defined, as required by C11 §7.29.1
// (note that the variables themselves are not used)
wchar_t wc = WCHAR_MIN | WCHAR_MAX;
wint_t wi = WEOF | WINT_MIN | WINT_MAX;
struct tm t;

#ifdef TEST_IMPLEMENTATION
#include "wchar.c"
#endif

volatile int nondet;

int main() {
  wchar_t *sc1 = L"Needle";
  wchar_t *sc2 = L"Haystack";
  wchar_t buf[20];
  wchar_t c = nondet;
  size_t n = 5, r;
  int i = -1;
  wchar_t *res = 0;

  if (nondet) { res = wmemchr(sc1, c, n); goto exit; }
  if (nondet) { i = wmemcmp(sc1, sc2, n); goto exit; }
  if (nondet) { res = wmemcpy(buf, sc2, n); goto exit; }
  if (nondet) { res = wmemmove(buf, sc2, n); goto exit; }
  if (nondet) { res = wmemset(buf, c, n); goto exit; }
  if (nondet) { res = wcschr(sc1, c); goto exit; }
  if (nondet) { i = wcscmp(sc1, sc2); goto exit; }
  if (nondet) { res = wcscpy(buf, sc2); goto exit; }
  if (nondet) { r = wcscspn(sc1, sc2); goto exit; }
  if (nondet) { r = wcslcat(buf, sc2, n); goto exit; }
  if (nondet) { r = wcslcpy(buf, sc2, n); goto exit; }
  if (nondet) { r = wcslen(sc1); goto exit; }
  if (nondet) { i = wcsncmp(sc1, sc2, n); goto exit; }
  if (nondet) { res = wcsncpy(buf, sc2, n); goto exit; }
  if (nondet) { res = wcspbrk(sc1, sc2); goto exit; }
  if (nondet) { res = wcsrchr(sc1, c); goto exit; }
  if (nondet) { r = wcsspn(sc1, sc2); goto exit; }
  if (nondet) { res = wcsstr(sc1, sc2); goto exit; }

  for (i = 0; i < 5; i++) buf[i] = nondet;
  buf[i] = L'\0';
  if (nondet) { res = wcscat(buf, sc2); goto exit; }
  buf[i] = L'\0';
  if (nondet) { res = wcsncat(buf, sc2, n); goto exit; }

  // invalid conversions char -> wchar_t, must be detected and not crash
  if (nondet) { r = wcslen((wchar_t*)""); goto exit; }
  if (nondet) { r = wcslen((wchar_t*)"A"); goto exit; }
  if (nondet) { r = wcslen((wchar_t*)"ABCD"); goto exit; }

  if (nondet) { res = wcschr((wchar_t*)"", L'A'); goto exit; }
  if (nondet) { res = wcschr((wchar_t*)"A", L'A'); goto exit; }
  if (nondet) { res = wcschr((wchar_t*)"ABCD", L'A'); goto exit; }

  // small sanity tests
  wchar_t *wc = L"ABC";
  wchar_t *p = L"ABC" + 1;
  wchar_t *wcr = wcschr(p, L'C');
  wchar_t *wmr1 = wmemchr(wc, L'C', 2); // not found
  wchar_t *wmr2 = wmemchr(p, L'C', 2); // found

exit:
  return 0;
}
