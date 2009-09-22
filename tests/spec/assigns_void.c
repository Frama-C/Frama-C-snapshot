/* run.config
 OPT: -print -journal-disable -continue-annot-error
 OPT: -val -main g -print -no-annot -journal-disable
 */
//@ assigns *x;
void f(void *x);

void g() {
  int y;
  int* x = &y;
  f(x);
}
