/* run.config
*    GCC:
*    OPT: -pdg-debug "-fct-pdg main" -inout -journal-disable
*/
/*
 * ledit bin/toplevel.top  tests/slicing/no_body.c -pdg-debug "-fct-pdg main"
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
