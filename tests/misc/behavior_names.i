/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/

/*@ behavior foo: ensures \true; */
void f () {
  int x = 0;
  /*@ behavior bar: ensures \true; */
  x++;
  if (x) { /*@ behavior bli: ensures \true; */ x++; }
}
