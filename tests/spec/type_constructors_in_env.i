/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/

/*@ type foo = A | B; */

/*@ logic foo f(integer x) = x>=0 ? A : B; */
