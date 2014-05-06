/* run.config
   STDOPT: +"-big-ints-hex 257" +"-inout-callwise"
*/
#include "../../share/builtin.h"

extern int b1, b2, b3, b4, b5, b6;

//@ ensures \initialized(&t[1..n-2]);
void f(int m, int* t, int n) {
  if (m)
    for (int i=1;i<n-1;i++)
      t[i]=i;
}

volatile rand;

void g1 (){
  int t1[20], t2[20], t3[20], t4[20], t5[20], t6[20], i, j;
  for (int i=0; i<20; i++)
    if (rand) { t1[i]=1; t2[i]=2; t3[i]=3; t4[i]=4; t5[i]=5; t6[i]=6; }
  //@ assert \initialized(&t1[..]);
  //@ assert \initialized(&t2[4..]);

  i=Frama_C_interval(3,6);
  j=Frama_C_interval(12,15);
  //@ assert \initialized(&t3[i..j]);

  i=Frama_C_interval(3,7);
  j=Frama_C_interval(7,15);
  //@ assert \initialized(&t4[i..j]);

  i=Frama_C_interval(7,9);
  j=Frama_C_interval(4,6);
  //@ assert \initialized(&t5[i..j]);

  i=Frama_C_interval(7,9);
  j=Frama_C_interval(4,7);
  //@ assert \initialized(&t6[i..j]);
}

void g2() {
  int t[14];
  if (b4) {
    t[0]=0x11223344;
    t[1]=t[0];
    t[2]=0x55667788;
    t[3]=t[2];
    if (b5) t[4]=0x12345678; else t[4] = 0x23456789;
    t[5]=t[4];
    t[6]=(int)&b4+(int)&b4;
    t[7]=t[6];
    t[8] = b5 ? 1 : 2;
    t[9] = t[8];
    t[10] = 0;
    t[11] = 0;
    int *p = (char*)(&t[10])+3;
    *p = 0x11111111;
    t[12] = 0;
    t[13] = 0;
    p+=2;
    *p = b5 ? 0x11111111: 0x22222222;
  }
  Frama_C_dump_each();

  int *p = ((char*)t)+7;
  //@ assert \initialized(p);

  Frama_C_dump_each();

  Frama_C_show_each(*p);   // assert *p == 0x66778811;

  //@ assert \initialized(p+2);

  //@ assert \initialized(p+4);

  //@ assert \initialized(&t[9]);

  //@ assert \initialized(&t[11]);

  //@ assert \initialized(&t[13]);
}

void g3() {
  int r1, x1, x2, r2, x3, r3;
  int t1[5];
  int t2[250];

  if (b1) x1 = 1;
  //@ assert \initialized(&x1);
  r1 = x1+1;

  if (b2)
    x2 = r2 + 1;

  if (b3) x3 = 1;
  r3 = x3 + 1;

  f(b6, &t1, 4);
  f(b6, &t2, 250);
}

void g4() {
  int x, y, z;
  x = y + z; // Do not continue evaluating z after y (or the converse)
             // without checking for bottom.
}

struct s {
  char a;
  int b;
};

/*@ assigns p->a, p->b \from \nothing;
  ensures \initialized(p); */ // Wrong because of padding
void wrong_assigns(struct s *p);

struct v {
  char a;
  char b;
};

struct v v1;

void g5() {
  struct s v;
  if (rand)
    wrong_assigns(&v);
  struct v v2;

  //@ assert \initialized(&v1);
  //@ assert !\initialized(&v2);
  struct v *p = rand ? &v1 : &v2;
  //@ assert \initialized(p);
}

int main () {
  g1();
  g2();
  g3();
  if (rand) g4();
  g5();
  return 0;
}
