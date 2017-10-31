/* run.config
   STDOPT: +"-cpp-extra-args=-DTEST_IMPLEMENTATION=1" +"-slevel 1000"
   STDOPT: +"-slevel 1000"
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

  if (nondet) res = wmemchr(sc1, c, n);
  if (nondet) i = wmemcmp(sc1, sc2, n);
  if (nondet) res = wmemcpy(buf, sc2, n);
  if (nondet) res = wmemmove(buf, sc2, n);
  if (nondet) res = wmemset(buf, c, n);
  if (nondet) res = wcschr(sc1, c);
  if (nondet) i = wcscmp(sc1, sc2);
  if (nondet) res = wcscpy(buf, sc2);
  if (nondet) r = wcscspn(sc1, sc2);
  if (nondet) r = wcslcat(buf, sc2, n);
  if (nondet) r = wcslcpy(buf, sc2, n);
  if (nondet) r = wcslen(sc1);
  if (nondet) i = wcsncmp(sc1, sc2, n);
  if (nondet) res = wcsncpy(buf, sc2, n);
  if (nondet) res = wcspbrk(sc1, sc2);
  if (nondet) res = wcsrchr(sc1, c);
  if (nondet) r = wcsspn(sc1, sc2);
  if (nondet) res = wcsstr(sc1, sc2);

  for (i = 0; i < 5; i++) buf[i] = nondet;
  buf[i] = L'\0';
  if (nondet) res = wcscat(buf, sc2);
  buf[i] = L'\0';
  if (nondet) res = wcsncat(buf, sc2, n);


  return 0;
}
