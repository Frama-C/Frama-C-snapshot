/* run.config
   OPT: -load-script tests/misc/behavior_names.ml
*/

/*@ behavior foo: ensures \true; */
void f () {
  int x = 0;
  /*@ behavior bar: ensures \true; */
  x++;
  if (x) { /*@ behavior bli: ensures \true; */ x++; }
}
