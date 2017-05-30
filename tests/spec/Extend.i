/* run.config
EXECNOW: make @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@ -copy -continue-annot-error
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
  /*@ loop foo i<=x;
      loop baz \at(i,LoopEntry), 0;
   */
  for (int i = 0; i < x; i++) s+=g(i);
  return s;
}

/*@ behavior ko:
  baz \true;
*/
int h(int z);
