/* run.config
   EXECNOW: make -s tests/slicing/select_simple.cmxs
   CMD: @frama-c@ -load-module tests/slicing/libSelect.cmxs -load-module tests/slicing/select_simple.cmxs
   OPT: -check -deps -journal-disable
*/

/* dummy source file in order to test select_simple.ml */

#include "tests/slicing/simple_intra_slice.c"
