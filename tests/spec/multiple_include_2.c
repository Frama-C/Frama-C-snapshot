/* run.config
   OPT: -kernel-warn-key=annot-error=active -print tests/spec/multiple_include_1.c -journal-disable
*/
#include "multiple_include.h"

/*@ requires p(x); */
void bar(int x) { i+=x; return; }
