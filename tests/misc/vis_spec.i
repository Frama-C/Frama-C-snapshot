/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/

//@ assigns \nothing;
void g (void) ;

//@ assigns \nothing;
void f () { g(); }

