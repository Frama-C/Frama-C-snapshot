/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -json-compilation-database @PTEST_DIR@ -print
OPT: -json-compilation-database @PTEST_DIR@/with_arguments.json -print
OPT: -json-compilation-database @PTEST_DIR@/with_arguments.json -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/
#include <stdio.h>

#ifdef TOUNDEF
#error TOUNDEF must be undefined by the compilation database
#endif

int main () {
  char *s = DOUBLE_SINGLE("a ");
  #ifndef __FRAMAC__
  printf("%s\n", s); // for GCC debugging
  #endif
  return MACRO_FOR_INCR(TEST); }
