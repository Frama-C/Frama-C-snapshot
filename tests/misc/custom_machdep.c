/* run.config*
OPT: -cpp-extra-args="-I@PTEST_DIR@/@PTEST_NAME@ -D__FC_MACHDEP_CUSTOM" -load-script @PTEST_DIR@/@PTEST_NAME@/@PTEST_NAME@.ml -machdep custom -print -then -print
COMMENT: we need a -then to test double registering of a machdep
*/

#include "__fc_machdep_custom.h"
#include "limits.h"

int main() { return INT_MAX; }
