/* run.config
   OPT: -val -deps -journal-disable
*/

int e;

/*@
  behavior a: ensures \result >> 2 == x;
  behavior b: ensures e >> 2 == x;
*/
int f(int x) {
  int y = 4 * x;
  /*@ assert y == x << 2; */
  e = y;
  return y;
}

int main() {
  f(42);
  return 0;
}
