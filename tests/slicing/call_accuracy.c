/* run.config
   OPT: -check -calldeps -slice-return main -slicing-level 3 -journal-disable -then-on 'Slicing export' -print
 */
int f_cond (int c, int a, int b) {
  ++a;
  ++b;
  return c ? a : b;
}

int test_cond (int x, int y, int z) {
  ++x ;
  ++y ;                   // <- best can be done.
  ++z ;
  z = f_cond(1, x, y);    // <- best can be done.
  ++y ;                   // <- best can be done.
  ++x ;
  ++z;
  return f_cond(0, y, z); // <- best can be done.
}



void f_set (int *p, int v) {
  *p = v ;
}

struct st {int a, b ;} S1, S2, S3, S4;
int test_set (int x, int y, int z) {
  int a, b ;
  struct st s1, s2, s3 = {1, 2} ;
  int tab [5] ;
  f_set(&a,x);
  f_set(&b,y);
  f_set(&s1.a,x);   // <- best++ can be done.
  f_set(&s1.b,y);
  f_set(&s2.a,x);
  f_set(&s2.b,z);
  f_set(&s3.a,x);
  f_set(&tab[0],x); // <- best++ can be done.
  f_set(&tab[1],y);
  f_set(&tab[2],z); // <- best++ can be done.
  f_set(&S1.a,x);   // <- best++ can be done.
  f_set(&S1.b,y);
  f_set(&S2.a,x);   // <- best++ can be done.
  f_set(&S2.b,y);
  S3.a = 1 ;        // <- best++ can be done.
  S3.b = 2 ;
  S4.a = 3 ;
  return b + s1.b + s3.b + tab[1] + S1.b + S2.b;
}

int test_struct (void) {
  S1.a = 1 ; // <- best can be done.
  S1.b = 2 ;
  S2.a = 3 ;
  S2 = S1 ;
  S2.a = 4 ; // <- best can be done.
  return S2.b ;

}
int main (int x, int y, int z) {
  int r1 = test_struct () ;
  int r2 = test_cond(x, y, z) ;
  int r3 = test_set (x, y, z) ;
  return r1 + r2 + r3 + S3.b + S4.b ;
}
