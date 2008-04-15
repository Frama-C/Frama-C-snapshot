/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -main f -journal-disable
*/
char a;
int b,c;
int f(void) {
  a = 'a';
  b = 97;
  c = a - b;
}
