/* run.config
STDOPT: +"@PTEST_DIR@/static_formals_2.c" +"-cpp-extra-args=\"-I @PTEST_DIR@\"" +"-kernel-msg-key printer:vid"
*/

#include "static_formals.h"

int g() { return f(4); }
