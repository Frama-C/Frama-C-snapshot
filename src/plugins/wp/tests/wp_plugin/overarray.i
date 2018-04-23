
//@ assigns p[0..9];
void g(int *p);

int A[20];

/*
  In this test we try to over-assign parts of A via g.  Remark: g shall require
  validity of p[0..9], which is NOT true in most cases.
 */

//@ assigns A[0..9];
void f1_ok(void) { g( A ); }

//@ assigns A[10..19];
void f2_ok(void) { g( A + 10 ); }

//@ assigns \nothing ;
void f3_ok(void) { g( A + 20 ); }

//@ assigns \nothing ;
void f4_ok(void) { g( A - 10 ); }

//@ assigns \nothing ;
void f5_ko(void) { g( A + 15 ); }

//@ assigns \nothing ;
void f6_ko(void) { g( A - 5 ); }
