/* run.config
   GCC:
   STDOPT: +"-occurrence"
*/

int x, y;

int main(int z) {
  int *p = &x, *q;
  *p = 0;
  /*@ assert (x == 0); */
  q = &y;
  p = q;
  *q = 1;
  *p = 2;
  /*@ assert (y == 2 && *q == 2 && *p == 2 && x == 0); */
  return z;
}
