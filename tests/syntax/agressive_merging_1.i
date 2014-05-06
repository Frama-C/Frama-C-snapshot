/* run.config
   STDOPT: +"tests/syntax/agressive_merging_2.i -agressive-merging"
*/
static inline void f(void) {
  return;
 }

void foo (void)
{
  f();
}
