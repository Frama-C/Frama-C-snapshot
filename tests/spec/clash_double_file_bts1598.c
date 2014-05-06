/* run.config
COMMENT: checks that linking string.h and its FC-pretty-printed version
COMMENT: does not get rejected by name clash in the logic. See bts 1598
OPT: @PTEST_FILE@ -cpp-extra-args " -Ishare/libc -nostdinc" -print -then -ocode @PTEST_DIR@/result/foo.c -print -then @PTEST_FILE@ @PTEST_DIR@/result/foo.c -ocode="" -print
*/

#include "__fc_builtin.h"
#include "assert.h"
//#include "complex.h"
#include "ctype.h"
#include "errno.h"
//#include "fenv.h"
#include "float.h"
#include "getopt.h"
#include "inttypes.h"
#include "iso646.h"
#include "limits.h"
#include "locale.h"
#include "math.h"
#include "setjmp.h"
#include "stdbool.h"
#include "stddef.h"
#include "signal.h"
#include "stdarg.h"
#include "stdint.h"
#include "stdio.h"
#include "stdlib.h"
#include "string.h"
//#include "tgmath.h"
#include "time.h"
#include "uchar.h"
#include "wchar.h"
#include "wctype.h"
