/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@ -print -constfold
*/

int main(void) {
  /* test Cil.constFoldBinOp called by mkBinOp for '%':
     the sign of the result is the sign of the divident */
  int res = 3 % 2 == -1; // 0
  res = 3 % -2 == -1;    // 0
  res = -3 % 2 == 1;     // 0
  res = -3 % -2 == 1;    // 0
  return res;
}
