/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/

int main(void) {
  /*@ assert P1: \true; */;
  /*@ assert P2: \true; */;
  /*@ assert P3: \true; */;
  /*@ assert P4: \true; */;
  return 0;
}
