/* run.config
   GCC:
   OPT: -val -deps -out -input  -main any_int -journal-disable
*/
int any_int() {
  volatile int y=0;
  int x=0;

  while(y) {y++;y++; if (y-1) x++; else x--;}
  return x;
}
