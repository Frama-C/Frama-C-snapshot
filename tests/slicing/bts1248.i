/*  run.config
OPT: -check -slice-rd x -main f -slicing-project-name p -then-on 'p export' -val
*/

int f(void) {
  int x = 0;
  return x; // <- cannot be selected since x is a local variable
}
