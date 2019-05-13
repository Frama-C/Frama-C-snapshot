/* run.config*
   STDOPT: +"-eva-verbose 2"
*/

/*@
  assigns \result \from a, b;
  ensures \result > 0;

  behavior b1:
    assumes a == 0;
    assigns \result \from a;
    ensures \result == 4 || \result == 5;

  behavior b2:
    assumes a != 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 2;

  behavior b3:
    assumes a != 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 3;

  behavior b4:
    assumes a == 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 4;

  behavior b5:
    assumes a == 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 5;

 */
int f_none(unsigned int a, unsigned int b) ;

/*@
  assigns \result \from a, b;
  ensures \result > 0;

  behavior b1:
    assumes a == 0;
    assigns \result \from a;
    ensures \result == 4 || \result == 5;

  behavior b2:
    assumes a != 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 2;

  behavior b3:
    assumes a != 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 3;

  behavior b4:
    assumes a == 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 4;

  behavior b5:
    assumes a == 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 5;

  complete behaviors b1, b2, b3;
  complete behaviors b2, b3, b4, b5;
  disjoint behaviors b1, b2, b3;
  disjoint behaviors b2, b3, b4, b5;
 */
int f_comp_disj(unsigned int a, unsigned int b) ;

/*@
  assigns \result \from a, b;
  ensures \result > 0;

  behavior b1:
    assumes a == 0;
    assigns \result \from a;
    ensures \result == 4 || \result == 5;

  behavior b2:
    assumes a != 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 2;

  behavior b3:
    assumes a != 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 3;

  behavior b4:
    assumes a == 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 4;

  behavior b5:
    assumes a == 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 5;

  disjoint behaviors b1, b2, b3;
  disjoint behaviors b2, b3, b4, b5;
 */
int f_disj(unsigned int a, unsigned int b) ;


/*@
  assigns \result \from a, b;
  ensures \result > 0;

  behavior b1:
    assumes a == 0;
    assigns \result \from a;
    ensures \result == 4 || \result == 5;

  behavior b2:
    assumes a != 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 2;

  behavior b3:
    assumes a != 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 3;

  behavior b4:
    assumes a == 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 4;

  behavior b5:
    assumes a == 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 5;

  complete behaviors b1, b2, b3;
  complete behaviors b2, b3, b4, b5;
 */
int f_comp(unsigned int a, unsigned int b) ;


/*@
  assigns \result \from a, b;
  ensures \result > 0;

  behavior b1:
    assumes a == 0;
    assigns \result \from a;
    ensures \result == 4 || \result == 5;

  behavior b2:
    assumes a != 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 2;

  behavior b3:
    assumes a != 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 3;

  behavior b4:
    assumes a == 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 4;

  behavior b5:
    assumes a == 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 5;

  complete behaviors b1, b2, b3;
  disjoint behaviors b1, b2, b3;
 */
int f_123_comp_disj(unsigned int a, unsigned int b) ;


/*@
  assigns \result \from a, b;
  ensures \result > 0;

  behavior b1:
    assumes a == 0;
    assigns \result \from a;
    ensures \result == 4 || \result == 5;

  behavior b2:
    assumes a != 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 2;

  behavior b3:
    assumes a != 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 3;

  behavior b4:
    assumes a == 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 4;

  behavior b5:
    assumes a == 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 5;

  complete behaviors b1, b2, b3;
  disjoint behaviors b1, b2, b3;
  disjoint behaviors b2, b3, b4, b5;
 */
int f_123_comp_disj_2345_disj(unsigned int a, unsigned int b) ;


/*@
  assigns \result \from a, b;
  ensures \result > 0;

  behavior b1:
    assumes a == 0;
    assigns \result \from a;
    ensures \result == 4 || \result == 5;

  behavior b2:
    assumes a != 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 2;

  behavior b3:
    assumes a != 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 3;

  behavior b4:
    assumes a == 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 4;

  behavior b5:
    assumes a == 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 5;

  complete behaviors b1, b2, b3;
  complete behaviors b2, b3, b4, b5;
  disjoint behaviors b1, b2, b3;
 */
int f_123_comp_disj_2345_comp(unsigned int a, unsigned int b) ;


/*@
  assigns \result \from a, b;
  ensures \result > 0;

  behavior b1:
    assumes a == 0;
    assigns \result \from a;
    ensures \result == 4 || \result == 5;

  behavior b2:
    assumes a != 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 2;

  behavior b3:
    assumes a != 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 3;

  behavior b4:
    assumes a == 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 4;

  behavior b5:
    assumes a == 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 5;

  complete behaviors b2, b3, b4, b5;
  disjoint behaviors b1, b2, b3;
  disjoint behaviors b2, b3, b4, b5;
 */
int f_123_disj_2345_comp_disj(unsigned int a, unsigned int b) ;


/*@
  assigns \result \from a, b;
  ensures \result > 0;

  behavior b1:
    assumes a == 0;
    assigns \result \from a;
    ensures \result == 4 || \result == 5;

  behavior b2:
    assumes a != 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 2;

  behavior b3:
    assumes a != 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 3;

  behavior b4:
    assumes a == 0 && b == 0;
    assigns \result \from a, b;
    ensures \result == 4;

  behavior b5:
    assumes a == 0 && b != 0;
    assigns \result \from a, b;
    ensures \result == 5;

  complete behaviors b1, b2, b3;
  complete behaviors b2, b3, b4, b5;
  disjoint behaviors b2, b3, b4, b5;
 */
int f_123_comp_2345_comp_disj(unsigned int a, unsigned int b) ;

volatile int nondet;

void test_none() {
  int a, b, c1, c2, c3, c4;
  a = nondet; // volatile
  b = nondet; // volatile

  if (!a && !b) c1 = f_none(a, b);
  if (!a && b)  c2 = f_none(a, b);
  if (a && !b)  c3 = f_none(a, b);
  if (a && b)   c4 = f_none(a, b);
}

void test_comp_disj() {
  int a, b, c1, c2, c3, c4;
  a = nondet; // volatile
  b = nondet; // volatile

  if (!a && !b) {
    // True behaviors in this call: <def>, b1, b4
    // Unknown behaviors in this call: none
    c1 = f_comp_disj(a, b);
    // at runtime: {4}
    // best approximation: {4}
  }

  if (!a && b) {
    // True behaviors in this call: <def>, b1
    // Unknown behaviors in this call: b4, b5
    c2 = f_comp_disj(a, b);
    // at runtime: {4}
    // best approximation: {4; 5}
  }

  if (a && !b) {
    // True behaviors in this call: <def>
    // Unknown behaviors in this call: b1, b2, b4
    c3 = f_comp_disj(a, b);
    // at runtime: {2}
    // best approximation: {2; 4}
  }

  if (a && b) {
    // True behaviors in this call: <def>
    // Unknown behaviors in this call: b1, b2, b3, b4, b5
    c4 = f_comp_disj(a, b);
    // at runtime: {3}
    // best approximation: {2; 3; 4; 5}
  }
}

void test_disj() {
  int a, b, c1, c2, c3, c4;
  a = nondet; // volatile
  b = nondet; // volatile

  if (!a && !b) c1 = f_disj(a, b);
  if (!a && b)  c2 = f_disj(a, b);
  if (a && !b)  c3 = f_disj(a, b);
  if (a && b)   c4 = f_disj(a, b);
}

void test_comp() {
  int a, b, c1, c2, c3, c4;
  a = nondet; // volatile
  b = nondet; // volatile

  if (!a && !b) c1 = f_comp(a, b);
  if (!a && b)  c2 = f_comp(a, b);
  if (a && !b)  c3 = f_comp(a, b);
  if (a && b)   c4 = f_comp(a, b);
}

void test_123_comp_disj() {
  int a, b, c1, c2, c3, c4;
  a = nondet; // volatile
  b = nondet; // volatile

  if (!a && !b) c1 = f_123_comp_disj(a, b);
  if (!a && b)  c2 = f_123_comp_disj(a, b);
  if (a && !b)  c3 = f_123_comp_disj(a, b);
  if (a && b)   c4 = f_123_comp_disj(a, b);
}

void test_123_comp_disj_2345_disj() {
  int a, b, c1, c2, c3, c4;
  a = nondet; // volatile
  b = nondet; // volatile

  if (!a && !b) c1 = f_123_comp_disj_2345_disj(a, b);
  if (!a && b)  c2 = f_123_comp_disj_2345_disj(a, b);
  if (a && !b)  c3 = f_123_comp_disj_2345_disj(a, b);
  if (a && b)   c4 = f_123_comp_disj_2345_disj(a, b);
}

void test_123_comp_disj_2345_comp() {
  int a, b, c1, c2, c3, c4;
  a = nondet; // volatile
  b = nondet; // volatile

  if (!a && !b) c1 = f_123_comp_disj_2345_comp(a, b);
  if (!a && b)  c2 = f_123_comp_disj_2345_comp(a, b);
  if (a && !b)  c3 = f_123_comp_disj_2345_comp(a, b);
  if (a && b)   c4 = f_123_comp_disj_2345_comp(a, b);
}

void test_123_disj_2345_comp_disj() {
  int a, b, c1, c2, c3, c4;
  a = nondet; // volatile
  b = nondet; // volatile

  if (!a && !b) c1 = f_123_disj_2345_comp_disj(a, b);
  if (!a && b)  c2 = f_123_disj_2345_comp_disj(a, b);
  if (a && !b)  c3 = f_123_disj_2345_comp_disj(a, b);
  if (a && b)   c4 = f_123_disj_2345_comp_disj(a, b);
}

void test_123_comp_2345_comp_disj() {
  int a, b, c1, c2, c3, c4;
  a = nondet; // volatile
  b = nondet; // volatile

  if (!a && !b) c1 = f_123_comp_2345_comp_disj(a, b);
  if (!a && b)  c2 = f_123_comp_2345_comp_disj(a, b);
  if (a && !b)  c3 = f_123_comp_2345_comp_disj(a, b);
  if (a && b)   c4 = f_123_comp_2345_comp_disj(a, b);
}

/*@
  assigns \result \from p, q;

  behavior b1:
    assumes p != \null;
    assigns \result \from p;

  behavior b2:
    assumes p == \null && q != \null;
    assigns \result \from q;

  behavior b3:
    assumes p == \null && q == \null;
    assigns \result \from \nothing;

  complete behaviors;
  disjoint behaviors;
 */
int f(int *p, int *q) ;

void test_assigns() {
  int a, b;
  int *p1, *p2, *p3;

  p1 = (int*)f(&a, &b); // garbled_mix of &{a}
  p2 = (int*)f(0, &b); // garbled_mix of &{b}
  p3 = (int*)f(0, 0); // [0..+oo]
}

char T[10];
/*@
  requires \valid(out+(0 .. l-1)) ;
  assigns out[0 .. l-1] \from \nothing ;

  behavior b:
    // no assigns clause (reverts to the default behavior's)
*/
void f2(char * out, unsigned int l) ;

void test_assigns2 () {
  char *p = T;
  f2(p, 5);
}

/*@
  requires x == 2 || x == 4;
  behavior b1:
    assumes x == 2;
    requires x == 0;
  behavior b2:
    assumes x == 4;
  complete behaviors;
 */
int f3(int x);

void test_small1() {
  int x = nondet;
  f3(x);
}

/*@
  requires x == 2 || x == 4;
  behavior b1:
    assumes x == 2;
    requires x == 0; requires x != 1; ensures \false;
  behavior b2:
    assumes x == 4;
 */
int f4(int x);

void test_small2() {
  int x = nondet;
  f4(x);
}

/*@
  ensures \result >= 0 && \result <= 1000;
  behavior b1:
    assumes x == 0;
  behavior b2:
    assumes x != 0;
 */
int f5(int x);

void test_small3() {
  int r = f5(nondet);
}

/*@
  ensures \result >= 0 && \result <= 1000;
  behavior b1:
    assumes x == 0;
  behavior b2:
    assumes x != 0;
  complete behaviors;
 */
int f6(int x);

void test_small4() {
  int r = f6(nondet);
}

/*@
  requires x >= 0 && x <= 50;
  behavior b1:
    assumes x > 0 && x != 50;
  behavior b2:
    assumes x == 0 || x >= 50;
 */
int f7(int x);

void test_small5() {
  int r = f7(nondet);
}

/*@
  requires \valid(p);
  assigns \result \from p;
  assigns *p \from \nothing;
  ensures \initialized(\result);
  behavior b:
    ensures \result == p;
 */
int *f8(int *p);

void test_small6() {
  int a;
  int *p = f8(&a);
}

/*@
  requires \valid(p);
  assigns \result \from p;
  assigns *p \from \nothing;
  ensures \result == p;
  behavior b:
    ensures \initialized(\result);
 */
int *f8_bis(int *p);

void test_small6_bis() {
  int a;
  int *p = f8_bis(&a);
}

/*@ axiomatic MyLen { type Lstr = char *; logic ℤ length{L}(Lstr s); } */
/*@
  assigns \result \from *s, n;
  behavior b1:
    assumes !\valid_read(s);
    ensures \result == -1;
  behavior b2:
    assumes \valid_read(s) && length(s) > 0;
    ensures \result == 1;
  complete behaviors;
 */
int f9(char const* s, int n);

void test_promote() {
  int x = nondet;
  int r = f9("a", 1);
}

/*@ axiomatic MyF { type Lint = int; logic ℤ fl{L}(Lint x); } */
/*@
  assigns \result \from x;
  behavior b1:
    assumes fl(x) > 0;
    ensures \result == 300 || \result == 500;
  behavior b2:
    assumes fl(x) <= 0;
    ensures \result == 100 || \result == 200;
  behavior b3:
    assumes fl(x) < 0;
    ensures \result == 100 || \result == 400;
  behavior b4:
    assumes fl(x) >= 0;
    ensures \result == 200 || \result == 300;

  complete behaviors b1, b2;
  complete behaviors b3, b4;
 */
int f10(int x);

void test_narrow() {
  int r = f10(nondet);
}

int main() {
  test_none();
  test_comp_disj();
  test_comp();
  test_disj();
  test_123_comp_disj();
  test_123_comp_disj_2345_disj();
  test_123_comp_disj_2345_comp();
  test_123_disj_2345_comp_disj();
  test_123_comp_2345_comp_disj();
  test_assigns();
  test_assigns2();
  test_small1();
  test_small2();
  test_small3();
  test_small4();
  test_small5();
  test_small6();
  test_small6_bis();
  test_promote();
  test_narrow();
}
