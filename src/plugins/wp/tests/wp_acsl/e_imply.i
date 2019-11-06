/* run.config
   OPT: -wp-gen -wp-print
*/

//@ axiomatic P { predicate P(integer x); }
//@ axiomatic T { predicate T = \true ; }
//@ axiomatic F { predicate F = \false; }

/*@ ensures p0: T;
  @ ensures p1: P(1) ==> T;
  @ ensures p2: F ==> P(2);
  @ ensures p3: F ==> P(2) ==> P(3);
  @ ensures p4: P(1) && P(2) ==> (P(3) ==> P(2)) && (P(3) ==> P(1));

  @ ensures p5: P(2) ==> (T && P(2));
  @ ensures p6: P(1) ==> P(2) ==> (P(1) && P(2));
  @ ensures p7: (F && P(12)) ==> (P(21) && P(22));
  @ ensures p8: P(1) ==> (P(1) ? T : P(3));
  @ ensures p9: P(1) && P(2) ==> (P(2) ? P(1) : P(3));

  @ ensures i0: (P(1) && P(2) && P(3)) ==> P(2);
  @ ensures i1: P(1) ==> P(2) ==> (P(1) && P(2) && T);
  @ ensures i2: F ==> P(1) && P(2) ==> (!P(1) || !P(2));
  @ ensures i3: P(1) && P(2) ==> (!P(1) || !P(2) || T);

  @ ensures i4: P(0) && P(1) && P(2) &&  P(3) && P(4) ==> (P(1) && P(3) && (P(2) || P(4)));
  @ ensures i5: P(0) && P(1) && P(2) ==> P(3) && P(4) ==> (P(1) && P(3) && (P(2) || P(4)));
  @ ensures i6: P(0) && P(1) && P(2) &&  P(3) && P(4) ==> (P(1) && P(3) && (P(2) && P(4) ==> T));
  @ ensures i7: P(0) && P(1) && P(2) ==> P(3) && P(4) ==> (P(1) && P(3) && (P(2) && P(4) ==> T));
  @ ensures i8: P(0) && P(1) && P(2) &&  P(3) && !P(5) ==> (P(1) && P(3) && (P(2) && F ==> P(5)));
  @ ensures i9: P(0) && P(1) && P(2) ==> P(3) && !P(5) ==> (P(1) && P(3) && (P(2) && F ==> P(5)));

  @ ensures a0: F && P(2) ==> !P(2) ;
  @ ensures a1: F ==> (P(1) && F && P(3)) ;
  @ ensures a2: F && !P(3) ==> P(0) && P(2) && P(4) ==> (F && P(2) && P(3)) ;
  @ ensures a3: F && !P(3) ==> P(0) && P(2) && P(4) ==> (F && P(2) && P(3)) ;
  @ ensures a4: !T ==> P(1) && P(0) && P(2) && P(4) ==> (P(1) && P(2) && T) ;
  @ ensures a5: !T ==> P(2) ==> (T && P(2) && P(3)) ;
  @ ensures a6: !T ==> P(2) && P(4) ==> (T && P(2) && P(3)) ;
  @ ensures a7: !T ==> P(2) ==> (T && P(2) && P(3)) ;
  @ ensures a8: F && !P(2) && P(4) ==> (F && P(2) && P(3)) ;
  @ ensures a9: F && P(2) && !P(3) && P(4) ==> (F && P(2) && P(3)) ;

  @ ensures o0: F && P(2) ==> (!P(2) || (F && P(3)));
  @ ensures o1: F ==> P(2) ==> (!F || !P(2) || !P(3)) ;
  @ ensures o2: F && P(3) ==> P(0) && P(2) && !P(4) ==> (!F || !P(2) || !P(3)) ;
  @ ensures o3: P(1) && !P(3) ==> P(0) && P(2) && P(4) ==> (!P(1) || !P(2) || !P(3)) ;
  @ ensures o4: !T ==> P(1) && P(0) && P(2) && P(4) ==> (P(1) || P(2) || !T) ;
  @ ensures o5: !T ==> P(2) ==> (T || !P(2) || P(3)) ;
  @ ensures o6: !T ==> P(2) && P(4) ==> (T || !P(2) || P(3)) ;
  @ ensures o7: !T ==> P(2) ==> (T || !P(2) || P(3)) ;
  @ ensures o8: F && !P(2) && P(4) ==> (!F || P(2) || !P(3)) ;
  @ ensures o9: F && P(2) && !P(3) && P(4) ==> (!F || !P(2) || P(3)) ;

  @ ensures f0: P(1) ==> (P(2) && (P(1) || P(12)) ==> T) ;
  @ ensures f1: P(1) && (P(2) || P(11)) ==> (P(2) && (P(1) || P(12)) ==> T) ;



*/
void f(void) { ; }
