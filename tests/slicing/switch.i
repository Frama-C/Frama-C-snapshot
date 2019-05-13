/*  run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   CMD: @frama-c@ -load-module tests/slicing/libSelect.cmxs -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: @EVA_OPTIONS@ -deps -journal-disable
*/
int main (char choix) {
  int x = 0, y = 0, z = 0;
  switch (choix) {
    case 'a' : x = 1; break;
    case 'b' : x = 2; y = 1; break;
    case 'c' :
    case 'd' : y = 2; break;
  }
  z++;
  return x;
}
