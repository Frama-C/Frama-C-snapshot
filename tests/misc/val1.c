/* run.config
   GCC:
   OPT: -memory-footprint 1 -val -deps -out -input -main h -journal-disable
   OPT: -memory-footprint 1 -val -deps -out -input -main g -journal-disable
   OPT: -memory-footprint 1 -val -deps -out -input -main f -journal-disable

*/

int x;
void f () {
  x = 7;
  x = x+x+x+1;
}

int x1,c1;
void g() {
  x1=0;
  if (c1) x1 = 1;
}

int x2,y2,z2;
void h() {
  x2=0;
  y2=1;
  z2= x2+y2;
//  while (1) {  z2 = z2 + 1;}
}
