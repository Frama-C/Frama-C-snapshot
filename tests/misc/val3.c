/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -main f -journal-disable
*/
int x,*b;

int f() {
  x = 777;
  b = &x;
  *b=0;
  return 0;
}
