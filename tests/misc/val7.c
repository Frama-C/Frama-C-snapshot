/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -main f
*/

#define NULL ((char*)0)
int a,b;
int f(void) {
  a = 'a';
//  NULL[(int)&a] = 1;
  *(char *) &a = 1;
  b = (char)a;
}
