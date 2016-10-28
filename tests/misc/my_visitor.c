/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
EXECNOW: LOG my_visitor_sav.res LOG my_visitor_sav.err BIN my_visitor.sav FRAMAC_PLUGIN=./lib/plugins @frama-c@ @PTEST_FILE@ -load-module @PTEST_DIR@/@PTEST_NAME@ -main f -save @PTEST_DIR@/@PTEST_NAME@.sav > @PTEST_DIR@/result/@PTEST_NAME@_sav.res 2> @PTEST_DIR@/result/@PTEST_NAME@_sav.err
OPT: -load @PTEST_DIR@/@PTEST_NAME@.sav -print
*/
int f() {
  int y = 0;
  y++;
  /*@ assert y == 1; */
  return 0;
}
