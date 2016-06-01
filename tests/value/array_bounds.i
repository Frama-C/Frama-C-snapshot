/* run.config*
   STDOPT: #"-main main"
   STDOPT: #"-unsafe-arrays"
*/

volatile int c;

struct { int a; int T[12]; int b; } s = {1,0,1,2,3,4,5,6,7,8,9,10,11,20};
struct { int a; int T[12]; int b; } u = {1,0,1,2,3,4,5,6,7,8,9,10,11,20};

void main() {
  s.a = 9;
  s.b = 9;
  for(int i=0; i+5<17;  i++) {
    Frama_C_show_each(i);
    u.T[i] = c;
  }
  if (c) {
    for(int j=0; j+5<=17; j++) {
      s.T[j] = c; // Invalid
    }
    u.a = -1;
  }

}
