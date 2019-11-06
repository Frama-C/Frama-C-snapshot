/* run.config
   MODULE: @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -no-autoload-plugins
*/

/*@ behavior foo: ensures \true; */
void f () {
  int x = 0;
  /*@ behavior bar: ensures \true; */
  x++;
  if (x) { /*@ behavior bli: ensures \true; */ x++; }
}
