/* run.config
   GCC:
   OPT: -occurrence
*/

int x, y;

int main() {
  int *p = &x, *q;
  *p = 0;
  /*@ assert (x == 0); */
  q = &y;
  p = q;
  *q = 1;
  return 0;
}
