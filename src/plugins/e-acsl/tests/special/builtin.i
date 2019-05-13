/* run.config
  LOG: gen_builtin.c
  OPT: -e-acsl-builtins incr -machdep gcc_x86_64 -check -e-acsl -then-last -load-script tests/print.cmxs -print -ocode tests/special/result/gen_@PTEST_NAME@.c
  EXECNOW: ./scripts/testrun.sh builtin special "1" "--frama-c=@frama-c@ -F -e-acsl-builtins=incr"
  LOG: gen_builtin2.c
  OPT: -e-acsl-builtins incr -machdep gcc_x86_64 -check -e-acsl -e-acsl-gmp-only -then-last -load-script tests/print.cmxs -print -ocode tests/special/result/gen_builtin2.c
  COMMENT: -e-acsl-builtins
*/

int incr(int);

/*@ ensures \result == incr(i); */
int f(int i) { int j = i + 1; return j; }

int incr(int x) { return x + 1; }

int main() {
  int i = f(2);
  return 0;
}
