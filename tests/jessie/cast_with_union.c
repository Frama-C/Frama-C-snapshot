/* run.config
   DONTRUN: cast no working yet
*/

union int_union {
  struct { char c1; char c2; char c3; char c4; } ic;
  struct { short s1; short s2; } is;
  int i;
};

// axiom ic_zero: \forall union int_union u; u.i == 0 ==> u.ic.c1 == 0;
/*@ axiomatic Zero {
  @ axiom ic_zero{L}: \forall union int_union* u; u->i == 0 ==> u->ic.c1 == 0;
  @ }
  @ */

void f(union int_union u) {
  u.i = 0;
  //@ assert u.ic.c1 == 0;
}

typedef union {
  int i; char c;
} uni;

void g() {
  uni u;
  u.i = 1;
  //@ assert u.i == 1;
  u.c = 2;
  //@ assert u.c == 2;
  //@ assert u.i == 2;
  u.i = 0;
  //@ assert u.c == 2; // wrong on purpose
}

typedef union {
  int i2; char uc2;
} uni2;

/*@ axiomatic Zero2 {
  @ axiom ic_zero2{L}: \forall uni2* u; u->i2 == 0 ==> u->uc2 == 0;
  @ axiom ic_zero3{L}: \forall uni2* u; *(int*)u == 0 ==> u->uc2 == 0;
  @ }
  @ */

void h() {
  uni2 u;
  int* p = &u.i2;
  *p = 0;
  //@ assert u.uc2 == 0;
  u.uc2 = 0;
}


/* 
Local Variables:
compile-command: "LC_ALL=C make cast_with_union"
End:
*/
