/* run.config
   OPT: -print -load-script ./tests/spec/bts0578.ml
*/

/*@ behavior foo: ensures \true; */
void main(void) {
  int i, t[10];
  /*@ loop assigns t[0..i];
      for foo: loop assigns t[0..i];
  */
  for (i = 0; i < 10; i++) { t[i] = 0; }
}
