/* run.config
*    GCC:
*    OPT: -fct-pdg main -inout -journal-disable  -pdg-print -pdg-verbose 2
*/
/*
 * ledit bin/toplevel.top  tests/slicing/no_body.c -fct-pdg main
 * #use "tests/slicing/select.ml";;
 * test "loop" (select_data "G");;
*/

int G;

int f (int a);

void loop (int x) {
  while (f(x)) {
    x++;
    G++;
  }
}

void main (void) {
  int x = 1;
  G = f(x);
  loop (x);
}
