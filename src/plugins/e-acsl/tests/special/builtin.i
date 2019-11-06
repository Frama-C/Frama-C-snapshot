/* run.config_ci
  COMMENT: -e-acsl-builtins
  LOG: gen_builtin.c
  STDOPT: #"-e-acsl-builtins incr"
*/

int incr(int);

/*@ ensures \result == incr(i); */
int f(int i) { int j = i + 1; return j; }

int incr(int x) { return x + 1; }

int main() {
  int i = f(2);
  return 0;
}
