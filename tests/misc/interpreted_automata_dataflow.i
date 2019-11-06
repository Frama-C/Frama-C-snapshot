/* run.config
OPT: -load-script tests/misc/interpreted_automata_dataflow.ml
*/

/* Tests the dataflow functor of interpreted automata via a caml script
   implementing a propagation of constants. */

void main(int x)
{
  int y = 3;
  y = y * 2;

  int z = y + 1;
  int w = y + x;
  int a = 1;

  for (int i = 0 ; i < 10 ; i ++) {
    int b = 3;
    int c = i + 1;
    a = a + 1;
  }

  if (x != 3)
    x = 3;
}

