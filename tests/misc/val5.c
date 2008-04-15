/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -main f -journal-disable
   OPT: -memory-footprint 1 -val -deps -out -input -main g -journal-disable
*/

int T[20];
int f() {
  ((int*)T)[1]=17;
  ((char*)T)[1]=18;
}

int g() {
  ((int*)T)[0]=10;
  ((char*)T)[1]=11;
}
