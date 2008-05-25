/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -main f
*/

int x,*b,a;

int f() {
  x = 777;
  a = (int)&x;
  b = (int*) a;
  *((int*)a) = 0;
  *b=*b+1;
}
