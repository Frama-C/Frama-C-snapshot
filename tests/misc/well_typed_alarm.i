/* run.config*
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/
int main(int c) {
  int x = 0;
  int y = 0;
  int *p = &x;
  int *q = &y;
  if (c) q = &x;
  if (p<=q) x++;
  return *q;
}
