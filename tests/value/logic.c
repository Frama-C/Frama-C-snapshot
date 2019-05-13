#include <__fc_builtin.h>
int t[10], u[11];
struct ts { int f1; int f2; } s1, s2, s3[10];
unsigned int x; volatile v;

struct s1{
  int x;
};

struct s2{
  struct s1 str;
};

struct S { int x; int y; };

struct T { int z; int t; };

struct T t_T;

/*@ axiomatic axio_1 {
  @   logic integer f_acsl (integer x) = x + 1;
  @   logic integer h_acsl (integer x) = 1 + h_acsl(x-1);
  @   logic integer mutual1{L1, L2}(int* x, int *y) = \at(*x*2, L1) + \at(*y+3, L2);
  @   logic integer mutual2{L1, L2}(int *v, int *w) = mutual1{L2, L1}(w, v);
  @   logic integer mute{L}(int x) = \at(x,L); // The label is actually unused
  @}
*/


/*@ predicate pred_1 (integer x) = 0 <= x < 100;*/
/*@ predicate pred_2{L1,L2}(integer x) = \at(x,L1) == \at(x,L2);*/
/*@ predicate pred_3{L1}(integer x) = \at(x,L1) == \at(x,L1);*/
/*@ predicate pred_4(struct s1 s) = s.x ==0;*/
/*@ predicate pred_5(integer a, integer b) = a + 1 == b;*/
/*@ predicate pred_6{L1,L2}(integer a, integer b) = \at(a,L1) + 1 == \at(b,L2);*/
/*@ predicate pred_7(integer a, integer b) = a == b && pred_7(a,b);*/
/*@ predicate pred_8(int x) = x == 5;*/
/*@ predicate pred_9(struct S s) = s.x + s.y == 42; */

void f (){
  struct s1 temp_1;
  struct s2 temp_2;

  temp_1.x = 0;
  temp_2.str.x = 0;
  /*@ assert pred_4(temp_1);*/
  /*@ assert pred_4(temp_2.str);*/
  return;
}

void g (){
  int x = 0;
  L:;
  int y = 1;
  /*@ assert pred_5(x,y);*/
  /*@ assert pred_5(x,x+1);*/
  /*@ assert pred_6{Here,Here}(x,y);*/
  /*@ assert pred_6{L,Here}(x,y);*/
  return;
}

void h (){
  int x = 0;
  int y = 0;
  int k = 5;
  int j = 6;
  int *p = &k;
  int *q = &j;
  /*@ assert x == h_acsl(x);*/
  /*@ assert pred_7(x,y);*/
  /*@ assert pred_8(*p);*/
  if (v) { /*@ assert pred_8(*q);*/ /* False */ }
  //@ assert mutual2{Here, Here}(p, q) == 5+3 + 2*6;
  // Make sure that k and s1.f1 are evaluated in Here despite the label!
  //@ assert mute{Pre}(k) == 5;
  s1.f1 = 1; //@ assert ! (mute{Pre}(s1.f1) == 0);
}

void unsup (){
  t_T.z = 21; t_T.t = 21;
  /*@ assert pred_9((struct S)t_T);*/
  return;
}

void pred(){
  int x = 10;
  int y = 0;
  L:;
  y = x + 1;
  /*@ assert y == f_acsl(x);*/
  /*@ assert pred_1(y);*/
  /*@ assert pred_2{L,Here}(x);*/
  /*@ assert pred_3{Here}(x);*/
  f();
  g();
  unsup();
  h();
  return;
}

void eq_tsets () {

 //@ assert \union() == \union();

  //@ assert \union(1) == \union(1);
  //@ assert \union(1, 2) == \union(1, 2);
  //@ assert \union(2, 1) == \union(1, 2);
  //@ assert ! (\union(1, 2) == \union(1, 3));
  //@ assert ! (\union(1, 2) == \union(1));
  //@ assert ! (\union(1 ,2) == \union(3, 4));
  //@ assert \union(1, 2) != \union(1, 3);
  //@ assert \union(1, 2) != \union(1);
  //@ assert \union(1 ,2) != \union(3, 4);

  //@ assert \union(x, x+1) != \union(-1, -3);

  //@ assert \union(1.0) == \union(1.0);

  //@ assert \union(&t) == \union(&t);
  //@ assert ! (\union(&t[0..1]) == \union(&t[0..2]));
  //@ assert ! (\union(&t[0..1]) == \union(&t[2..3]));
  //@ assert (\union(&t[0..1]) == \union(&t[0..1]));

  // Seems to be OK according to the typing given by the kernel. The WP is also happy
  //@ assert \union(\union(1,2)) == \union(\union(1), \union(2));
  //@ assert \union(\union(1,2)) == \union(\union(1), 2);
  //@ assert \union(\union(1,2)) == \union(1, 2);
  //@ assert \union(\union(1,1)) == \union(\union(1), 1);

  //@ assert &s3[0..1].f2 != 0;
  //@ assert &s3[0 .. -1].f1 != &s3[0..1].f2;
  //@ assert &s3[0 .. 1].f1 == &s3[0..1].f1;

  //@ assert s1 == s2; // True at link-time
  //@ assert t != u; // false

  //@ assert \union(0) == \union(0.0); 
  //@ assert \union(1.0) == \union(1);
  //@ assert \union(1, 1.0) == \union(1.0, 1);

  //@ assert \union() != \union(x);

  //@ assert \inter(&t, &u) == \empty;

}

void eq_char() {
  char c = '\x82'; // equal to 130. Very different from \130 which is in octal
  Frama_C_show_each(c);
  //@ assert c == '\x82';
  //@ assert c == 130-256;
}

void casts() {
  //@ assert (float)5 == 5.;
  //@ assert (double)5 == 5.;
}

/*@ requires r1: \valid (input + (0..l-1));
    requires r2: \valid (&input[0..l-1]);
    assigns input[0..l-1] \from \nothing; */
void f_empty_tset (unsigned char * input, int l);

void empty_tset () {
  unsigned char T[1] = {2};
  f_empty_tset (T, 0);
  //@ assert T[0] == 2;
}

void reduce_by_equal() {
  int a[10];
  a[v] = v;
  //@ assert \initialized(&a[0..9]);
  //@ assert a[0..8] == 1; // This syntax is not recommended (use \subset instead), but works for == and !=;
}

// Check that "partial" arithmetic operators check their arguments.
// We cannot reduce either
void alarms () {
  //@ slevel 0;
  int x = v;
  //@ assert ASSUME: x == -1 || x == 1;

  //@ assert UNK: 1 << x == 2; // Does not hold because of -1. Cannot reduce, because 1 << -1 may be equal to 2
  Frama_C_show_each(x);
  //@ assert UNK: 2 >> x == 1;
  Frama_C_show_each(x);

  //@ assert ASSUME: x == 1;
  //@ assert OK: 1 << x == 2;
  Frama_C_show_each(x);
  //@ assert OK: 2 >> x == 1;
  Frama_C_show_each(x);


  x = v;
  //@ assert ASSUME: x == 0 || x == 1;
  //@ assert UNK: 1 / x == 1; // Does not hold because of 0
  Frama_C_show_each(x);
  //@ assert UNK: 1 % x == 0; // Does not hold because of 0
  Frama_C_show_each(x);

  //@ assert ASSUME: x == 1;
  //@ assert OK: 1 / x == 1;
  Frama_C_show_each(x);
  //@ assert OK: 1 % x == 0;
  Frama_C_show_each(x);
}

struct pair { int i1; int i2; };
/*@ assigns p == \null ? \empty : *p, q == \null ? \empty : *q, *out
    \from indirect:p, *p, indirect:q, *q;
    ensures p == \null ? (q == \null ? *out == 10 : *out == 20)
                       : (q == \null ? *out == 30 : *out == 40);
    behavior p_nonnull:
      assumes p != \null;
      ensures *p == \at(*p,Pre) + 1;
    behavior q_nonnull:
      assumes q != \null;
      ensures q->i1 == \at(q->i1,Pre) + 2;
      ensures q->i2 == \at(q->i2,Pre) + 3;
 */
void select_like(int *p, struct pair *q, int *out);

void cond_in_lval() {
  int a = 3, out = 0;
  struct pair b = {4, 5};
  select_like(0, 0, &out);
  //@ assert out == 10;
  select_like(0, &b, &out);
  //@ assert b.i1 == 6;
  //@ assert b.i2 == 8;
  //@ assert out == 20;
  select_like(&a, 0, &out);
  //@ assert a == 4;
  //@ assert out == 30;
  a = 3;
  b.i1 = 4; b.i2 = 5;
  select_like(&a, &b, &out);
  //@ assert a == 4 && b.i1 == 6 && b.i2 == 8;
  //@ assert out == 40;
  int x = v;
  /*@ assert x >= 0 ? x >= 0 : x < 0; */
  x = x;
  //@ assert x > 2 ? x > 2 : \true;
  if (x) //@ assert 1 ? x <= 0 : x > 0; // must evaluate to unknown
    ;
}

void float_sign() {
  //@ assert \sign((float)0.0) == \Positive;
  //@ assert \sign((double)-0.0) == \Negative;
  //@ assert \sign((long double)0.0) != \Negative;
  //@ assert \Positive == \sign((float)0.0);
  double d = v ? 0.0 : -0.0;
  //@ assert \sign(d) == \Positive && \sign(d) == \Negative; // must be unknown
}


int *arr_ptr[3], arr_ptr_arr[6];

//@ assigns *(arr_ptr[0..2]) \from \nothing;
void assign_tsets_aux (void);

void assign_tsets () {
  arr_ptr [0] = &arr_ptr_arr[1];
  arr_ptr [1] = &arr_ptr_arr[4];
  arr_ptr [2] = &arr_ptr_arr[5];
  assign_tsets_aux (); // Make sure the under-approximation is precise: no "(and SELF)" information in froms
}


void min_max () {
  int x = Frama_C_interval(3, 17);
  int y = Frama_C_interval(1, 5);
  int z = Frama_C_interval(1, 100);
  int r1 = v;
  int r2 = v;
  int r3 = v;
  int r4 = v;
  //@ assert r1 == \max(x, y);
  //@ assert r2 == \max(x, z);
  //@ assert r3 == \min(x, y);
  //@ assert r4 == \min(x, z);
  double a = 0.;
  double b = - 0.;
  double d = v;
  //@ assert d == \min(a, b);
}

/* Tests assert and check assertions. */
void check_and_assert () {
  int x;
  x = v;
  /*@ assert x == 42; */
  Frama_C_show_each_42(x);
  /*@ check x == 42; */
  x = v;
  /*@ check x == 42; */
  Frama_C_show_each_imprecise(x);
  /*@ assert x == 42; */
  if (v) {
    /*@ assert x == 0; */
    Frama_C_show_each_unreachable(x); /* The assert led to bottom. */
  } else {
    /*@ check x == 0; */
    Frama_C_show_each_reachable(x); /* A check should never lead to bottom.  */
  }
}

void main () {
  eq_tsets();
  eq_char();
  casts();
  empty_tset();
  reduce_by_equal();
  alarms ();
  cond_in_lval();
  pred();
  float_sign();
  min_max();
  assign_tsets();
  check_and_assert ();
}
