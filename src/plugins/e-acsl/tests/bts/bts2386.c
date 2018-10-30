/* run.config, run.config_2
   COMMENT: pointer substraction
   COMMENT: on gcc_x86_64
   LOG: gen_@PTEST_NAME@.c
   OPT: -machdep gcc_x86_64 -e-acsl -then-last -load-script tests/print.cmxs -print -ocode tests/bts/result/gen_@PTEST_NAME@.c -kernel-verbose 0 -val -value-verbose 0
   COMMENT: on x86_32 (default case when no machdep is given)
   LOG: gen_@PTEST_NAME@_2.c
   OPT: -machdep x86_32 -e-acsl -then-last -load-script tests/print.cmxs -print -ocode tests/bts/result/gen_@PTEST_NAME@_2.c -kernel-verbose 0 -val -value-verbose 0
   COMMENT: with "-e-acsl-gmp-only"
   LOG: gen_@PTEST_NAME@_3.c
   OPT: -e-acsl -e-acsl-gmp-only -then-last -load-script tests/print.cmxs -print -ocode tests/bts/result/gen_@PTEST_NAME@_3.c -kernel-verbose 0 -val -value-verbose 0
*/

void f(const void *s, int c, unsigned long n) {
  const unsigned char *p = s;
  /*@ assert p - s == n - n; */
  /*@ assert p - s == 0; */
}

int main() {
  const char *s = "1234567890";
  f(s, '0', 11);
  return 0;
}