/* run.config
OPT: -load-script tests/misc/find_enclosing_loop.ml
*/

int f () {
  int x = 0;
  int y = 0;
  while (x<15) {
    x++;
    while (y<15) { y++; }
    x++;
    y =0;
  }
  x=0;
  y=0;
}
