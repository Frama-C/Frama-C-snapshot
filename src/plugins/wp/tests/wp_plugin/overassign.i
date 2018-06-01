
//@ assigns p[0..9];
void g(int *p);

int A[20];
int *p;

/*
  In this test we try to over-assign parts of A via g.  Remark: g shall require
  validity of p[0..9], which is NOT true in most cases.
 */

//@ requires p == A; assigns p[0..9];
void f1_ok(void) { g( p ); }

//@ requires p == A; assigns p[10..19];
void f2_ok(void) { g( p + 10 ); }

//@ requires p == A; assigns \nothing ;
void f3_ok(void) { g( p + 20 ); }

//@ requires p == A; assigns \nothing ;
void f4_ok(void) { g( p - 10 ); }

//@ requires p == A; assigns \nothing ;
void f5_ko(void) { g( p + 15 ); }

//@ requires p == A; assigns \nothing ;
void f6_ko(void) { g( p - 5 ); }
