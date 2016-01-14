/* run.config
   EXECNOW: make -s tests/slicing/min_call.cmxs
   CMD: @frama-c@ -load-module tests/slicing/libSelect.cmxs -load-module tests/slicing/min_call.cmxs
   OPT: -check -deps -lib-entry -main g -journal-disable -slicing-level 3
*/

/* dummy source file in order to test minimal calls feature
 * on select_return.c  */

#include "tests/slicing/select_return.c"
