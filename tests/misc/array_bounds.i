/* run.config
   OPT: -val -deps -out -input -journal-disable
   OPT: -unsafe-arrays -val -deps -out -input -journal-disable
*/

struct { int a; int T[5]; int b; } s = {1,0,1,2,3,4,5};

void main(int c) {
  s.a = 9;
  s.b = 9;
  for(int i=0; i+5<=10; i++) {s.T[i] = c;}

}
