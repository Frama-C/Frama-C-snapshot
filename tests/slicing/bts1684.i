/* run.config
   OPT: -check -slice-calls main -journal-enable -then-on 'Slicing export' -print
*/
// one bug about JOURNALIZATION and another one about slicing CALLS TO MAIN function.
double d1, d2, d3;
int x1, x2, x3;
int main2 (void) {
  d1 = d2 * d3;
  x1 = x2 * x3;
  return 1;
}

int main (void) {
  return main2();
}
