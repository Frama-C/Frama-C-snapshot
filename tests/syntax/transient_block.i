/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -kernel-warn-key transient-block=active
*/

void f(void) { }

int main () {

  int x = 1;
  x = 2;
  f();

}
