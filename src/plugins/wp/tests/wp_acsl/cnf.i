/* run.config
   DONTRUN:
*/

/* run.config_qualif
   OPT: -wp -wp-split-depth -2 -wp-par 1 -wp-msg-key cnf -wp-debug 1

// -wp-split-depth -2 -> replace the Goal by Goal<==>CNF(Goal)
*/


//@ axiomatic Ax { predicate A;  predicate A1;  predicate A2; }
//@ axiomatic Bx { predicate B;  predicate B1;  predicate B2; }
//@ axiomatic Cx { predicate C;  predicate C1;  predicate C2; }
//@ axiomatic Px { predicate P(integer x); }

/*@
  @ ensures a0: A && A1 && A2 ;
  @ ensures a1: A || A1 || A2 ;
  @ ensures a2: A && A1 ==> A2 ;
  @ ensures a3: A1 <==> A2 ;
  @ ensures a4: A ? A1 : A2 ;
  @ ensures a5: ( A && A1 && A2 ) || ( C && (B && B1 && B2) );
  @ ensures a6: ( A || A1 || A2 ) || ( C && (B || B1 || B2) );
  @ ensures a7: ( A && A1 ==> A2 ) || ( C && (B && B1 ==> B2) ) ;
  @ ensures a8: ( A1 <==> A2 ) || ( C && (B1 <==> B2) ) ;
  @ ensures a9: ( A ? A1 : A2 ) || ( C && (B ? B1 : B2) ) ;

  @ ensures b0: C && (B && B1 && B2) ;
  @ ensures b1: C && (B || B1 || B2) ;
  @ ensures b2: C && (B && B1 ==> B2) ;
  @ ensures b3: C && (B1 <==> B2) ;
  @ ensures b4: C && (B ? B1 : B2) ;
  @ ensures b5: ( C && (B && B1 && B2) ) ==> ( C1 || (B && B1 && B2) );
  @ ensures b6: ( C && (B || B1 || B2) ) ==> ( C1 || (B || B1 || B2) ) ;
  @ ensures b7: ( C && (B && B1 ==> B2) ) ==> ( C1 || (B && B1 ==> B2) ) ;
  @ ensures b8: ( C && (B1 <==> B2) ) ==> ( C1 || (B1 <==> B2 )) ;
  @ ensures b9: ( C && (B ? B1 : B2) ) ==> ( C1 || (B ? B1 : B2) ) ;

  @ ensures c0: C || (B && B1 && B2) ;
  @ ensures c1: C || (B || B1 || B2) ;
  @ ensures c2: C || (B && B1 ==> B2) ;
  @ ensures c3: C || (B1 <==> B2) ;
  @ ensures c4: C || (B ? B1 : B2) ; 
  @ ensures c5: ( C || (B && B1 && B2) ) <==> ( C1 <==> (B && B1 && B2) ) ;
  @ ensures c6: ( C || (B || B1 || B2) ) <==> ( C1 <==> (B || B1 || B2) ) ;
  @ ensures c7: ( C || (B && B1 ==> B2) ) <==> ( C1 <==> (B && B1 ==> B2) ) ;
  @ ensures c8: ( C || (B1 <==> B2 ) <==> ( C1 <==> (B1 <==> B2) )) ;
  @ ensures c9: ( C || (B ? B1 : B2) ) <==> ( C1 <==> (B1 <==> B2) ) ;
 
  @ ensures d0: C <==> (B && B1 && B2) ;
  @ ensures d1: C <==> (B || B1 || B2) ;
  @ ensures d2: C <==> (B && B1 ==> B2) ;
  @ ensures d3: C <==> (B1 <==> B2) ;
  @ ensures d4: C <==> (B ? B1 : B2) ;
  @ ensures d5: (C || B1 || B2) <==> (B && B1 && B2) ;
  @ ensures d6: (C || B1 || B2) <==> (B || B1 || B2) ;
  @ ensures d7: (C || B1 || B2) <==> (B && B1 ==> B2) ;
  @ ensures d8: (C || B1 || B2) <==> (B1 <==> B2) ;
  @ ensures d9: (C || B1 || B2) <==> (B ? B1 : B2) ;

  @ ensures e0: A && (((A || B) ==> C) || (B ==> C)) ;
  @ ensures e1: B && (((A || B) ==> C) || (B ==> C)) ;
  @ ensures e2: C && (((A || B) ==> C) || (B ==> C));
  @*/
void f(void) { ; }
