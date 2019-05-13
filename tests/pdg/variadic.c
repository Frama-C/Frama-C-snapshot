/* run.config
*    STDOPT: +"-pdg "
*/
// __builtin_va_start and co do not appear in PDG output because Value forgets to register them in the table of called functions. This is a bug of Value

#include <stdarg.h>

int f (int n, ...) {
  va_list arg;
  int i, s = 0;
  va_start (arg, n);
  for (i = 0; i < n; i++) {
    int x = va_arg (arg, int);
    s += x;
  }
  va_end (arg);
  return s;
}

int lib_f (int n, ...);

int f1 (int a) {
  return lib_f (1, a);
}

int f2 (int a, int b) {
  return lib_f (2, a, b);
}

int f3 (int a, int b, int c) {
  return lib_f (3, a, b, c);
}

int main (void) {
  int a1 = 1, a2 = 2, a3 = 3, a4 = 4, a5 = 5, a6 = 6;
  int s, s1, s2, s3;
  s1 = f1 (a1);
  s2 = f2 (a2, a3);
  s3 = f3 (a4, a5, a6);
  s = f2 (s1, s2);
  return s;
}
