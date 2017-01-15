/* run.config
 STDOPT: #"-machdep msvc_x86_64"
 */
// Note: machdep MSVC is used to avoid warnings due to
// "non implemented long double" when testing strtold.
// In MSVC, "long double" is mapped to "double".
#include <stdlib.h>

volatile int nondet;
int main() {
  int base = nondet ? 0 : nondet ? 2 : 36;
  char *sl = "12 34 -56";
  char *s = sl;
  char *pl, *q;
  long l = strtol(s, &pl, base);
  l = strtol(pl, &q, base);
  l = strtol(q, NULL, base);
  l = strtol(s+9, NULL, base);

  s = sl;
  char *pll;
  long long ll = strtoll(s, &pll, base);
  ll = strtoll(pll, &q, base);
  ll = strtoll(q, NULL, base);

  s = sl;
  char *pul;
  unsigned long ul = strtoul(s, &pul, base);
  ul = strtoul(pul, &q, base);
  ul = strtoul(q, NULL, base);

  s = sl;
  char *pull;
  unsigned long long ull = strtoull(s, &pull, base);
  ull = strtoull(pull, &q, base);
  ull = strtoull(q, NULL, base);

  char *sd = " 3.14 0x1.2p2";
  s = sd;
  char *pd;
  double d = strtod(s, &pd);
  d = strtod(pd, &q);
  d = strtod(q, NULL);

  s = sd;
  char *pld;
  long double ld = strtold(s, &pld);
  ld = strtold(pld, &q);
  ld = strtold(q, NULL);

  s = sd;
  char *pf;
  float f = strtof(s, &pf);
  f = strtof(pf, &q);
  f = strtof(q, NULL);

  return 0;
}
