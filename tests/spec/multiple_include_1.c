/* run.config
   DONTRUN: whole test is done in multiple_include_2.c
*/
#include "multiple_include.h"

/* @ logic integer li = 42; */

/*@ ensures p(\result); */
int foo() { return i; }
