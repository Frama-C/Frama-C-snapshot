typedef struct { int i1; char c1; } L1 ;

//@ ensures \result == p->i1;
int fits1(L1 * p) {
  int * q = (int *) p;
  return *q;
}

typedef struct { char c2; int i2; } L2;

//@ ensures \result == p->c2;
int mismatch1(L2 * p) {
  int * q = (int *) p;
  return *q;
}

typedef struct { char c3; L1 ic3; int i3; } L3 ;

//@ ensures \result == p->ic3.i1;
int fits2(L3 * p) {
  L2 * q = (L2 *) p;
  return q->i2;
}

typedef struct { char c4; L1 ic4[2]; int i4; } L4 ;

//@ ensures \result == p->ic4[0].i1;
int fits3(L4 * p) {
  L2 * q = (L2 *) p;
  return q->i2;
}

typedef struct { L3 ci5[2]; } L5 ;

typedef struct { L2 ci6[2]; char c6; } L6 ;

//@ ensures \result == p->ci5[1].c3;
int fits4(L5 * p) {
  L6 * q = (L6 *) p;
  return q->c6;
}

typedef union { int u7; } L7 ;

//@ ensures \result == p->u7;
int fits5(L7 * p) {
  int * q = (int *) p;
  return *q;
}

typedef union { int i8; char u8; double d8;} L8 ;
//@ ensures \result == p->i8;
int mismatch2(L8 * p) {
  int * q = (int *) p;
  return *q;
}

//@ ensures \result == *p;
int mismatch3(int * p) {
  L8 * q = (L8 *) p;
  return q->i8;
}
