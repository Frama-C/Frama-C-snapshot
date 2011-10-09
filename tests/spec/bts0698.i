/* -------------------------------------------------------------------------- */
/* --- Testing logic casts on array types                                 --- */
/* -------------------------------------------------------------------------- */

//@ predicate P(int x[2]) = x[0] < x[1] ;

//@ predicate Q{L}(int *x) = x[0] < x[1] ;

//@ predicate Correct{L}(int *x) = P((int[2]) x) ;

//@ predicate Incorrect{L}(int x[2]) = Q{L}((int *) x) ;

int t[2] ;
int * a ;

void f(void)
{
  t[0] = 10 ;
  t[1] = 20 ;
  //@ assert P(t) ;
  //@ assert Q((int *)t) ;
}

//@ requires \valid(a+(0..1)) ;
void g(void)
{
  a[0] = 10 ;
  a[1] = 20 ;
  //@ assert P((int[2])a) ;
  //@ assert Q(a) ;
}
