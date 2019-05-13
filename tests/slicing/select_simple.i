/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   CMD: @frama-c@ -load-module tests/slicing/libSelect.cmxs -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: @EVA_OPTIONS@ -deps -journal-disable
*/

/* dummy source file in order to test select_simple.ml */

#include "tests/slicing/simple_intra_slice.c"
