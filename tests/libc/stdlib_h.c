/* run.config
 STDOPT: #"-machdep msvc_x86_64"
 */
// Note: machdep MSVC is used to avoid warnings due to
// "non implemented long double" when testing strtold.
// In MSVC, "long double" is mapped to "double".
#include <stdlib.h>

//@ assigns \result \from *(int*)a, *(int*)b;
int compare_int(const void *a, const void *b) {
  return (*(int*)a < *(int*)b) ? -1 : (*(int*)a > *(int*)b);
}

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

  int ai[4] = {1, -1, 50000, 20};
  int key = 4;
  int *p = bsearch(&key, ai, 4, sizeof(int), compare_int);
  //@ assert p == \null;
  key = -1;
  p = bsearch(&key, ai, 4, sizeof(int), compare_int);
  //@ assert p == &ai[1];

  // tests for *env functions
  /*{
    char mutable[12] = "MUTABLE=yes";
    putenv(mutable);
    mutable[8] = 'n';
    mutable[9] = 'o';
    mutable[10] = 0;
    char *v = getenv("MUTABLE");
    if (v[8] != 'n') return 1; // possible only if imprecise
  }*/
  return 0;
}
