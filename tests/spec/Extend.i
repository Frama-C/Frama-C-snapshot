/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -copy -kernel-warn-key=annot-error=active
*/

/*@ foo x == 0;
    bar \result == 0;
    bla \trace(x<10) || \trace(x>40);
 */
int f(int x);

/*@ behavior test:
  foo y == 1;
  bar y + \result == 0;
  bla \trace(y<42) && \trace(y>12);
*/
int g(int y);


int f(int x) {
  int s = 0;
  /*@ loop lfoo i<=x;
      loop baz \at(i,LoopEntry), 0;
   */
  for (int i = 0; i < x; i++) s+=g(i);
  /*@ ca_foo s == 0; */
  return s;
}

/*@ behavior ko:
  baz \true;
*/
int h(int z);

int k(int z) {
  int x = z;
  int y = 0;
  /*@ ns_foo \at(x, Post) == z + 1; */
  y = x++;
  return y;
}

/*@ global_foo \forall integer x; x < x + 1
; */

//@ behavior ca_foo: ensures ca_foo: \true;
void loop (void) {
  //@ for ca_foo: ca_foo \true;
  //@ ns_foo \true;
  //@ baz \true;
  /*@ loop invariant \true; */
  while (0) { }
}
