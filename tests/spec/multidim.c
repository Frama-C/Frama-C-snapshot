typedef int TT[3][4] ;

TT ttt[5] ;

int (*pt)[3][4] = ttt ; // int (*)[3][4] and TT* are identical

/*@ axiomatic A {
  @ predicate P(TT * pt) ;
  @ predicate Q(int (*pt)[3][4] ) ;
  @ }
  @*/

//KO: implicit conversion from array to pointer
/*@ requires P(ttt) ;
 */
void g();

//OK: explicit conversion.
/*@ requires Q(&ttt[0]) ;
  @*/
void f() { }
