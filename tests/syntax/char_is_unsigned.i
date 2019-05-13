/* run.config
   EXECNOW: make -s @PTEST_DIR@/machdep_char_unsigned.cmxs
   OPT:-print -load-module @PTEST_DIR@/machdep_char_unsigned -machdep unsigned_char -then -constfold -rte
*/
char t[10];

void main() {
  int r = (t[0] == 'a');
  char c = 455;
}
