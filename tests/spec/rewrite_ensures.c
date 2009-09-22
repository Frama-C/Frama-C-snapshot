/* run.config_no_native_dynlink
   EXECNOW: make -s tests/spec/rewrite_ensures.cmo
   CMD: FRAMAC_PLUGIN=tests/spec bin/toplevel.byte
   OPT: -print
*/
/* run.config
   EXECNOW: make -s tests/spec/rewrite_ensures.cmxs
   CMD: FRAMAC_PLUGIN=tests/spec bin/toplevel.opt
   OPT: -print
*/

/*@
  requires x == 3;
  ensures x == 3; */
int f (int x) { x = 4; return x; }

/*@ ensures *x == 3; */
int g(int* x) { return *x = 3; }

/*@ ensures t[i] == 3; */
int h(int* t, int i) { return t[i] = 3; }

/*@ requires i <= 2;
  ensures t[i] == \old(t[i+1]);
*/
int f1(int* t, int i) {
  t[i] = t[i+1];
  return 0;
}
