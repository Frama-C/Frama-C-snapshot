/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   STDOPT: +"-load-module ./tests/slicing/libSelect.cmxs -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs -ulevel -1 -deps -slicing-level 2 -journal-disable"
*/

#include "tests/test/adpcm.c"
