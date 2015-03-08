/* run.config
EXECNOW: make @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@ -print -copy -check
*/

/*@ foo x == 0;
    bar \result == 0;
 */
int f(int x);

/*@ behavior test:
  foo y == 1;
  bar y + \result == 0;
*/
int g(int y);
