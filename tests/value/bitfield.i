/* run.config*
   STDOPT: +"-print -kernel-msg-key printer:bitfields"
*/

struct t1 { unsigned int a:2; int b:4; int c:22;int d:32;} h;
struct t2 { unsigned int a:2; int b:4; int c:22; int d;} k,k8,kr8;

struct t3 { int b:16; } ll;

struct t1 ini = { 14, -55, 99999 } ;

unsigned int VV=55;

unsigned short q4 = 40000;

int X;

void f(int x)
{
  X=x;
  Frama_C_dump_each();
}

int return_8(void)
{   
    return 8;
}

struct S { unsigned f:32; signed sf:32; } x = { 28349, 28349};
unsigned short us = 0xDC23L;
int G,H;
int g(void) {
  int r = (x.f ^ ((short)-87)) >= us;
  H = (x.sf ^ ((short)-87)) >= us ;
  return r;
}

union U1 {
   int f0 ;
   int f1 : 15 ;
};

struct impr {
  int i1: 5;
  int i2:1;
  int i3:6;
};

// Bug 1671

struct B {
  struct foo *next;
  struct foo **prev;
};

struct A {
  struct B next;
  int bitf:1;
} *b, *c, ee;

void leaf (struct A *p1);

volatile foo;

void imprecise_bts_1671 ()
{
    ee.next.prev = &b;
    c = &ee;
    while (foo)  {
      leaf (c);
      Frama_C_show_each(ee);
      c->bitf = 0;
      Frama_C_show_each(ee);
      c = c->next.next;
    }
}

struct bitf {
  unsigned v0_3: 3;
  unsigned v4: 1;
  unsigned v5_31: 28;
};

extern struct bitf y;

void logic() {
  if (y.v4) {
    y.v4 = 0;
    Frama_C_show_each(y);
  } else
  Frama_C_show_each(y);
  Frama_C_show_each(y.v4);
  //@ assert y.v4 == 0;

  Frama_C_show_each(*((unsigned *) &y));

  struct bitf w;
  w.v0_3 = 1;
  //@ assert \initialized(&w.v0_3);
  //@ assert ! \initialized(&w.v4);
  if (foo) {
    int wc = w.v4 + 1;
  }

  //@ assert \separated(&w.v0_3, &w.v4); // The status on this property will be invalid if the analysis evaluates the addresses using bytes
}

void main_old (){
  struct t1 v;

  union U1 l_161;
  l_161.f0 = (int)-1L;
  Frama_C_show_each(1);
  if ((!l_161.f0) <= l_161.f1) 
    Frama_C_show_each(2);
  else 
    Frama_C_show_each(3);

  VV = h.a;

  h.a = VV;

  v.c = (int) &v;
  v.d = (int) (&v + 1);
  v.d = v.d + 1;
  v.a = 4;
  v.b = 7;
  f(v.b);
  h.b = foo + foo + h.a + h.b;
  h.c = (int) (&v +1);

  k8.b = 8;
  kr8.b = return_8();

  ll.b = q4;
  G=g();

}

void eq_bitfields() {
  int i = foo;
  if (i >= 16 && i <= 17) {
    struct t1 s;
    s.a =i;
    s.c =i;
    Frama_C_dump_each();
    if (s.a == s.c) // False, because of bitfield downcasts
      Frama_C_show_each_unreachable();
  }
}

struct char_short {
  unsigned char c: 1;
  unsigned short s: 2;
};

void char_short () {
  struct char_short S = { 1, 2};
  int x = S.c + 0;
  int y = S.s + 0;
}

void main() {
  main_old();
  imprecise_bts_1671();
  logic();
  eq_bitfields();
  char_short();
}
