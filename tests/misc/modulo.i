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

void simultaneous_congruences(void)
{
  /* Different tests for x congruent to r1 mod m1 and to r2 mod m2. */

  /* Test with pgcd(m1,m2) = 1, r2-r1 = 1: a solution. */
  extern int i2;
  /*@ assert 0<= i2 <= 0x02000000 ; */

  int n1 = i2 * 13 + 7;
  int n2 = i2 * 15 + 8;
  int n3;

  if(n1 == n2) { n3 = n1;} else { while(1);}

  /* Test with pgcd(m1,m2) != 1, r2-r1 !=1, pgcd(m1,m2) does not
   * divide r2 -r1: no solution. */
  int m1 = i2 * 4 + 7;
  int m2 = i2 * 6 + 10;
  if(m1 == m2) { /*@ assert \false; */ }

  /* Test with pgcd != 1, r2-r1 !=1, pgcd(m1,m2) divides (r2-r1): a
   * solution. */
  int o1 = i2* 8 + 3;
  int o2 = i2* 12 + 11;
  int o3;
  if(o1 == o2) { o3 = o1;} else { while(1);}
}

void shift_modulo(void)
{
  /*@ assert 0 <= i <= 10; */
  int r = (i * 12 + 5) << 2;
  int s = ((i * 12 + 5) << 24)>>24;
  int q = ((i * 12 + 5) << 25)>>25;
  int t = ((i * 13 + 7) << 25)>>25;
}

void extract_bits_modulo(void)
{
  /*@ assert 0 <= i <= 10; */
  int aa1 = (i * 12 + 5) * 256 + 11;
  unsigned char *ptr1 = (unsigned char *)&aa1;
  int m1 = ptr1[0]; 		/* Ideally: congruent to 11 modulo 256; equal to 11. */
  int n1 = ptr1[1];               /* Ideally: congruent to 5 modulo 12. */

  int aa2 = (i * 12 + 5) * 256 + (i * 11 + 14);
  unsigned char *ptr2 = (unsigned char *)&aa2;
  int m2 = ptr2[0]; 		/* Ideally: congruent to 3 modulo 11. */
  int n2 = ptr2[1];             /* Ideally: congruent to 5 modulo 12. */

  int aa3 = (i * 12 + 5) * 256 + (i * 11 + 16);
  unsigned char *ptr3 = (unsigned char *)&aa3;
  int m3 = ptr3[0]; 		/* Ideally: congruent to 5 modulo 11. */
  int n3 = ptr3[1];             /* Ideally: congruent to 5 modulo 12. */

  int aa4 = (i * 11 + 16);
  unsigned char *ptr4 = (unsigned char *)&aa4;
  int m4 = ptr4[0]; 		/* Ideally: congruent to 5 modulo 11. */
  int n4 = ptr4[1];             /* Ideally: equal to zero. */
}

volatile int v;

// Test extraction of modulo with 'positive' semantics (ie. not nearest
// to zero in absolute value, which is the one '%' would have used). 
void pos_rem(void) {
  int n = v;
  //@ assert -1 <= n <= 255;

  int j = (int)*(signed char*)&n;

  n = v;
  //@ assert 0 <= n <= 135;
  int k = (int)*(unsigned char*)&n;

  n = v;
  //@ assert -1 <= n <= 72;
  int l = (int)*(signed char*)&n; // Best rem is ([0..72] \cup {255})%255, we approximate by [-128..127]
}

void main() {
  if (v) { pgcd1(a, b); }
  if (v) { pgcd2(a, b); }
  if (v) { pgcd3(a, b); }

  main2();
  simultaneous_congruences();
  shift_modulo();
  extract_bits_modulo();
  pos_rem();
}
