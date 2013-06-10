/* run.config
   STDOPT: +"-slevel-function pgcd1:100,pgcd2:100,pgcd3:100"
*/
int A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R;
extern int i;

void main2 ()
{
  A = (4 * i) % 4;
  B = (4 * i + 1) % 4;
  //@ assert ((i>=-100) && (i<=100)) ;
  E = (3*i + 1) % 12; 
  //@ assert ((i>=0) && (i<=100)) ;
    
  C = (4 * i + 1) % 4;
  D = (3*i + 1) % 12; 
  F = (24*i + 5) % 12;    
  G = (24*i + 5) % 13;    
  H = i % 1000;
  I = (2 * i+1101) % 1000;
  J = (5 * i - 201) % 1000;
  K = (5 * i - 201) % 10;

  L = K % J;
  M = K % D;
  N = J % I;
  O = I % G;
  P = A % J;
  Q = J % L;
}

extern int a, b;

/*@ requires -10<=x<=10 && -10<=y<=10; */
int pgcd1(int x, int y) {
  int a = x, b = y;
  /*@ loop invariant -10<=b<0||b==0||0<b<=10;
      loop invariant -10<=a<0||a==0||0<a<=10; */
  while(b!=0) {
    int tmp = a % b;
    Frama_C_show_each_1(a,b,tmp);
    a = b; b = tmp;
  }
  return a;
}

/*@ requires -10<=x<=10 && -10<=y<=10; */
int pgcd2(int x, int y) {
  int a = x, b = y;
  /*@ loop invariant -10<=b<0||b==0||0<b<=10; */
  while(b!=0) {
    int tmp = a % b;
    Frama_C_show_each_2(a,b,tmp);
    a = b; b = tmp;
  }
  return a;
}

/*@ requires -10<=x<=10 && -10<=y<=10; */
int pgcd3(int x, int y) {
  int a = x, b = y;
  while(b!=0) {
    int tmp = a % b;
    Frama_C_show_each_3(a,b,tmp);
    a = b; b = tmp;
  }
  return a;
}


volatile int v;

void main() {
  if (v) { pgcd1(a, b); }
  if (v) { pgcd2(a, b); }
  if (v) { pgcd3(a, b); }

  main2();
}
