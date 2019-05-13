/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/
//@ ensures *p==1;
void main(int * p){ *p = 0; }
