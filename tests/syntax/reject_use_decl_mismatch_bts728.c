/* run.config
STDOPT: +"-cpp-extra-args='-DHAS_PROTO'"
STDOPT: +"-cpp-extra-args='-DNO_PROTO'"
*/

#ifdef HAS_PROTO
int f();
#endif

int G;

int H;
int main () {
  int T=99;
  H= f(2);
  return T; /* gcc -O0 -> 26; gcc -O3 -> 99 */
}

int f(int x,int y, int z, int t,int t1,int t2,int t3,int t4,int t5,int t6) {
  x = 17;
  y=18;
  z=19;
  t=20;
  t1= 21;
  t2 = 22;
  t3 = 23;
  t4= 24;
  t5 = 25;
  t6 = 26;
  return x;
}
