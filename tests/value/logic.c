int t[10], u[11];
struct ts { int f1; int f2; } s1, s2, s3[10];
unsigned int x; volatile v;

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

void main () {
  eq_tsets();
  eq_char();
  casts();
  empty_tset();
  reduce_by_equal();
  alarms ();
  cond_in_lval();
}
