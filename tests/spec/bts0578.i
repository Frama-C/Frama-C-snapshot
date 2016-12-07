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

  L1: t[4] = 42;

  L2: t[3] = 36;

  L3: t[2] = 12;

}
