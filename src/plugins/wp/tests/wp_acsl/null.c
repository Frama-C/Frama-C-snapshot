/* run.config_qualif
   OPT: -wp -wp-par 1
*/

#define NULL ((void*)0)
//@ ensures \result == 0;
int null_is_zero (void) {
  void * p = NULL;
  return (int) p;
} 
