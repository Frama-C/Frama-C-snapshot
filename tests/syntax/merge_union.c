/* run.config
OPT: -cpp-extra-args="-I @PTEST_DIR@" @PTEST_DIR@/@PTEST_NAME@_2.c @PTEST_DIR@/@PTEST_NAME@_3.c -print
OPT: -cpp-extra-args="-I @PTEST_DIR@" @PTEST_DIR@/@PTEST_NAME@_2.c @PTEST_DIR@/@PTEST_NAME@_3.c -print -kernel-msg-key="-WARNING:link:drop-conflicting-unused"
*/

#include "merge_union.h"
int f(un* u);
