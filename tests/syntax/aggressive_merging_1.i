/* run.config
   STDOPT: +"@PTEST_DIR@/aggressive_merging_2.i -aggressive-merging"
*/
static inline void f(void) {
  return;
 }

void foo (void)
{
  f();
}
