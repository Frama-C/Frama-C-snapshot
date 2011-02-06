
/*
  model name : UnsafeCaveat
  kind : Positive
  bhv : first part Provable; last part not yet finished
*/

int X;
int *P;

void var_subst_const (void) {
  int x = 3;
    //@ assert ok: x == 3;
}

void var_subst_var (int x) {
  int y = x + 1;
  //@ assert ok: y == x + 1;
  x = x + 1;
  //@ assert ok: y == x;
}
void assign_ptr (void) {
  int x;
  int * p = &x;
  //@ assert ok: p == &x;
}
//@ ensures ok: \result == &X;
int * return_addr (void) {
  return &X;
}

//@ ensures should_be_ok: *p == 3;
void ptr_subst (int * p) {
  *p = 3;
  //@ assert ok: *p == 3;
}
void ptr_on_var (void) {
  int x = 0;
  int * p = &x;
  //@ assert ok1: *p == 0;
  *p = 3;
  //@ assert ok2: *p == 3;
  //@ assert not_supported: x == 3;
  //@ assert unsafe_err_ok: x == 0;
}
//@ ensures should_be_ok: \result == *p;
int read_ptr (int * p) {
  int y = *p;
  //@ assert ok: y == *p;
  return y;
}
int read_ptr_shift (int * p) {
  int i = 3;
  int y = *(p + i);
  //@ assert ok: y == *(p+3);
  return y;
}
void shift_ptr (int * p) {
  *p = 1;
L: p++;
  *p = 2;
  //@ assert ok1: *p == 2;
  //@ assert ok2: *(p-1) == 1;
  //@ assert not_supported: \at(p, Pre)[1] == 2;
  //@ assert ok3: \at(*p, L) == 1;
}

struct Tstr { int a; int b; int * p; int t[10]; };

void caveat_havoc_field (struct Tstr * p) {
  //@ assigns p->a;
  {
    p->a = 0;
  }
  //@ assert ok1: p->t[1] == \at(p->t[1], Pre);
  //@ assert ok2: p->b == \at(p->b, Pre);
  //@ assert KO: p->a == 0; // not provable this because of stmt spec
}

void caveat_havoc_shift (int * p) {
  //@  assigns p[0];
  {
    p[0] = 0;
  }
  //@ assert ok_with_caveat: p[1] == \at(p[1], Pre);
  //@ assert KO: p[0] == 0;
}
void caveat_havoc_range (int * p) {
  //@  assigns p[1..3];
  {
    p[1] = 0; p[3] = 0;
  }
  //@ assert ok_with_caveat: p[0] == \at(p[0], Pre);
  //@ assert KO: p[1] == 0;  // not provable this because of stmt spec
}
