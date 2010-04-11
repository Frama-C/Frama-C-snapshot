int C;

/*@ behavior ok :
      ensures C == \at(c, Pre);
      ensures \at(c, Pre) == c; // param cannot be modified by a function.
    behavior ko :
      ensures C == \at(C, Pre); // false : global var has been modified.
*/
void f0 (int x,int c) {
  C = c;
  c = x;
  c++;
  //@ assert x == c-1;
}

// simplest example of assignment...
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

/*@ ensures S.a == x ; 
    ensures S.p == \old(S.p) ;
    ensures S == { \old(S) for a = x };
    */
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
/*@ requires S.p == &S || \separated (S.p, &S);
  @ ensures S.p->a == x ;
  @ ensures \forall int i; (*(S.p)).t[i] == \old((*(S.p)).t[i]);
  */
void rw_ptr_field (int x) {
  S.p->a = x;
}

int T[10];

/*@ ensures T[i] == x;
    ensures \forall int j; i != j ==> T[j] == \old(T[j]);
*/
//TODO : T == { \old(T) for [i] = x}
void rw_array_elem (int i, int x) {
  T[i] = x;
}

int * P;

//@ ensures *P == x && P == \old(P);
void rw_pointer (int x) {
  *P = x;
}

/*@ requires \valid(P+i) && \separated(&P, P+i);
  @ ensures *(P + i) == x && P == \old(P);
  @ ensures \forall int j; i != j ==> *(P + j) == \old(*(P + j));
*/
void rw_shift_pointer (int i, int x) {
  *(P+i) = x;
}

//@ ensures \forall int k; k == i+j ==> *(P + k) == x;
void rw_shift_shift_pointer (int i, int j, int x) {
  *(P+i+j) = x;
}

// Notice that there is no real indirect access : should be checked by M0...
/*@ ensures T[i+j] == x;
  @ ensures *(T+(i+j)) == x;
  @ ensures *(T+i+j) == x;
*/
void rm_shift_array_elem (int i, int j, int x) {
  *(T+i+j) = x;
}

// No indirect access
//@ ensures \result == &(T[i]);
int * return_ptr (int i) {
  return T+i;
}

// No indirect access
//@ ensures P == T+(i+1);
void assign_pointer (int i) {
  P = T+i;
  P++;
}

// No indirect access
//@ ensures \result == &(S.t[1]);
int * return_St1 (void) {
  int * p = S.t;
  return p+1;
}

int main (void) { return 0 ; }
