/* run.config
   STDOPT: #"-machdep gcc_x86_32"
 */


#include <limits.h>

int main() {
  int res;
  _Bool r = __builtin_sadd_overflow(42, 43, &res);
  //@ assert res == 42 + 43;
  //@ assert r == 0;
  r = __builtin_sadd_overflow(42, INT_MAX, &res);
  //@ assert res == (int)(42 + INT_MAX);
  //@ assert r == 1;
  long lres;
  r = __builtin_saddl_overflow(42, LONG_MAX, &lres);
  //@ assert lres == (long)(42 + LONG_MAX);
  //@ assert r == 1;
  r = __builtin_saddl_overflow(-2, -LONG_MAX, &lres);
  //@ assert lres == (long)(-2 - LONG_MAX);
  //@ assert r == 1;
  long long llres;
  r = __builtin_saddll_overflow(-5, -LLONG_MAX, &llres);
  //@ assert llres == (long long)(-5 - LLONG_MAX);
  //@ assert r == 1;
  unsigned ures;
  r = __builtin_uadd_overflow(9, UINT_MAX, &ures);
  //@ assert ures == (unsigned)(9 + UINT_MAX);
  //@ assert r == 1;
  unsigned long ulres;
  r = __builtin_uaddl_overflow(9, ULONG_MAX, &ulres);
  //@ assert ulres == (unsigned long)(9 + ULONG_MAX);
  //@ assert r == 1;
  unsigned long long ullres;
  r = __builtin_uaddll_overflow(9, ULLONG_MAX, &ullres);
  //@ assert ullres == (unsigned long long)(9 + ULLONG_MAX);
  //@ assert r == 1;
  r = __builtin_usubll_overflow(-5, ULLONG_MAX, &ullres);
  //@ assert ullres == (unsigned long long)(-5 - ULLONG_MAX);
  //@ assert r == 1;
  r = __builtin_smulll_overflow(-1, LLONG_MIN, &llres);
  //@ assert llres == (long long)(-1 * LLONG_MIN);
  //@ assert r == 1;
  return 0;
}
