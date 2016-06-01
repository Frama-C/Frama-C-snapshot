/* run.config*
   STDOPT: +" -then -unsafe-arrays"
*/

volatile v;

struct st1 {
  int a;
  int b;
  int *pp;
  int *p;
};
int *outp;
int x,y,z1,z2,z3,z4;
struct st1 T[22] = { {1,2,0,&x}, {&z1,&z2,&z3,&y},{&z4,2,0,&x},{1,2,0,&x} };

struct S {
  int a;
  int t[7];
  int b;
};

struct S s = { 1, 2, 3, 4, 5, 6, 7, 8, 9}, s1, s2, s3;

//@ assigns s->t[5..] \from \nothing;  // Must not write on a or b
void f1(struct S *s);

//@ assigns s->t[..2] \from \nothing;  // Must not write on a or b
void f2(struct S *s);

//@ assigns s->t[..] \from \nothing;  // Must not write on a or b
void f3(struct S *s);

void main1 () {
  outp = T[v].p;
  *outp = 5;
  z1++;
}

void main2() { // Semantics of ACSL [..] in Trange
  Frama_C_show_each(s);
  s1 = s; f1(&s1);
  Frama_C_show_each(s1);
  s2 = s; f2(&s2);
  Frama_C_show_each(s2);
  s3 = s; f3(&s3);
  Frama_C_show_each(s3);

  if (v) {
    //@ assert \valid(&s.t[..]);  // True in -safe-arrays mode, false otherwise
    Frama_C_show_each_reach();
  }
}

void main() {
  main1();
  main2();
}
