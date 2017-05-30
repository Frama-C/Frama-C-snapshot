/* run.config*
OPT: -cpp-extra-args="-I@PTEST_DIR@/@PTEST_NAME@ -D__FC_MACHDEP_CUSTOM" -load-script @PTEST_DIR@/@PTEST_NAME@/@PTEST_NAME@.ml -machdep custom -print -then -print
COMMENT: we need a -then to test double registering of a machdep
*/

#include "__fc_machdep_custom.h"
// most of the following includes are not directly used, but they test if
// the custom machdep has defined the necessary constants
#include <ctype.h>
#include <inttypes.h>
#include <limits.h>
#include <locale.h>
#include <math.h>
#include <signal.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <wchar.h>

int main() { return INT_MAX; }
