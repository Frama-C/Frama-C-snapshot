int C;

//@ ensures \at(c, Pre) == C &&  \at(c, Pre) == c;
void f0 (int x,int c) {
  C = c;
  c = x;
  c++;
  //@ assert x == c-1;
}

int f1() {
  int x;
  x = 0;
  x++;
  //@ assert x == 1;
  return 0;
}

/*@ requires y == 12 ; */
int f2(int y) {
  int x;
  x = y;
  //@ assert x == 12;
  if (x == 0) x++; else x = 1;
  //@ assert x == 1;
  return 0;
}

//@ ensures (x < 3) ==> \result == 0;
int f3a (int x) {
  return (x > 3) ? 1 : 0;
}

//@ ensures (x < 3) ==> \result == 0;
int f3b (int x) {
  int c = (x > 3);
  return c;
}

/*@ ensures (x != 0) ==> \result == 1;
    ensures (x == 0) ==> \result == 0;
    */
int f3c (int x) {
  return x ? 1 : 0;
}

struct Ts {int x; int y; };
struct Tstr {int a; struct Ts s; int t[10]; struct Tstr * p; } S;

//@ ensures S.a == x && S.p == \old(S.p) && S == { \old(S) for a = x };
void rw_int_field (int x) {
  S.a = x;
}
//@ ensures S.s.x == x && S.s == { \old(S.s) for x = x };
void rw_field_field (int x) {
  S.s.x = x;
}
//@ ensures S.a == \old(S.a) && S.t[i] == x;
void rw_tab_field (int i, int x) {
  S.t[i] = x;
}
//@ ensures S.p->a == x && \forall int i; (*(S.p)).t[i] == \old((*(S.p)).t[i]);
void rw_ptr_field (int x) {
  S.p->a = x;
}

int T[10];

//@ ensures T[i] == x && \forall int j; i != j ==> T[j] == \old(T[j]);
//TODO : T == { \old(T) for [i] = x}
void rw_array_elem (int i, int x) {
  T[i] = x;
}

int * P;

//@ ensures *P == x && P == \old(P);
void rw_pointer (int x) {
  *P = x;
}

//@ ensures *(P + i) == x;
void rw_shift_pointer (int i, int x) {
  *(P+i) = x;
}

//@ ensures *(P + i) == x;
void rw_shift_shift_pointer (int i, int j, int x) {
  *(P+i+j) = x;
}

//@ ensures T[i+j] == x && *(T+i+j) == x && (T+i)[j] == x;
void rm_shift_array_elem (int i, int j, int x) {
  *(T+i+j) = x;
}
