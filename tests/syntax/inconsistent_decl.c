/* run.config
STDOPT: +"tests/syntax/inconsistent_decl_2.i"
STDOPT: +"tests/syntax/inconsistent_decl_2.i"+"-cpp-extra-args='-DWITH_PROTO'"
*/

#ifdef WITH_PROTO
int f();
#endif

int g() {
  int x = f(2);
  return x;
}
