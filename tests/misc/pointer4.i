/* run.config
   GCC:
   OPT: -val -deps -out -input -absolute-valid-range 0-0x7 -journal-disable
*/
unsigned short d,e[10]={0},c = 0;

void main(void) {

  ((int*)0x0)[1] = 1;
  ((int*)0x0)[0] = 2;
  d = 1;
  for (c=0; c<=10; c++){
    e[0] = 1;
    d=0;
    ((int*)0x0)[c] = 0;}
}
