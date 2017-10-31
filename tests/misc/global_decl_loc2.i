/* run.config
   OPT: @PTEST_DIR@/global_decl_loc.i -load-module @PTEST_DIR@/global_decl_loc.cmxs
*/

extern int g;

int main(void) {
  int a = g;
  return a;
}
