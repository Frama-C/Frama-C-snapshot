/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/

int x;

int f() {
  static int x = 0;
  x++;
  return x;
}

int g() {
  int x = 0;
  x++;
  return x;
}

int main () {
  x++;
  return x;
}
