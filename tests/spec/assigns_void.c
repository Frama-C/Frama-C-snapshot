/* run.config
 OPT: -print
 OPT: -val -main g -print -no-annot
 */
//@ assigns *x;
void f(void *x);

void g() {
  int y;
  int* x = &y;
  f(x);
}
