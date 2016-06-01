/* run.config
EXECNOW: make -s tests/misc/my_visitor_plugin/my_visitor.cmxs
EXECNOW: LOG my_visitor_sav.res LOG my_visitor_sav.err BIN my_visitor.sav FRAMAC_PLUGIN=./lib/plugins @frama-c@ @PTEST_FILE@ -load-module @PTEST_DIR@/my_visitor_plugin/my_visitor -main f -save @PTEST_DIR@/my_visitor.sav > @PTEST_DIR@/result/my_visitor_sav.res 2> @PTEST_DIR@/result/my_visitor_sav.err
OPT: -load @PTEST_DIR@/my_visitor.sav -print
*/
int f() {
  int y = 0;
  y++;
  /*@ assert y == 1; */
  return 0;
}
