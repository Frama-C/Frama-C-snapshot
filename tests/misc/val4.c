/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -main f
*/
int T[20],*b;

int f() {
  T[0] = 0;
  T[4] = 0;
  b = T;
  *b = 1;
}
