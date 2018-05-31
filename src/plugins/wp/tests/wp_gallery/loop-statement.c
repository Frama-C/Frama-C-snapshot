/* run.config
   OPT: -wp-no-print
*/

/* run.config_qualif
   OPT:
*/

typedef char Te; // type for loop inputs
typedef int  Ts; // type for loop outputs

/*@ axiomatic Ts_list {
  @   logic \list<Ts> empty_Ts_list = \Nil;
  @   logic \list<Ts> add2_Ts_tail(\list<Ts> x, Ts b) =
        \concat(x, \Cons(b, empty_Ts_list));
 @ } */

/*@ axiomatic Ploop {

  @   predicate Pcond( Te e, Ts a ) reads \nothing;
  @   predicate Pbody( Te e, Ts a, Ts b ) reads \nothing;
  @   predicate Piter( Te e, Ts a, Ts b ) = Pbody(e,a,b) && Pcond(e,a);

  @   predicate Pinv( \list<Ts> x, Te e, Ts i, Ts a ) =
          0 < \length(x)
       && i == \nth(x, 0)
       && a == \nth(x, \length(x)-1)
       && \forall integer k ;
              0 <= k < \length(x)-1 ==> Piter(e, \nth(x, k), \nth(x, k+1) );

  @   predicate Pinduc( Te e, Ts a, Ts b ) =
      \forall Ts i, \list<Ts> x ;
          Pinv(x, e, i, a)
      ==> Pinv( add2_Ts_tail(x,b), e, i, b );

  @   lemma Lb:
      \forall Te e, Ts a, b ;
          Piter(e, a, b)
      ==> Pinduc( e, a, b );

  @   predicate Ploop( Te e, Ts i, Ts b ) =
         \exists \list<Ts> x ; Pinv( x, e, i, b ) ;

 @ } */

/*@ assigns \nothing;
  @*/
void nop(void);

Ts G; // Loop outputs

/*@ assigns \nothing;
  @ ensures Cond: \result != 0 <==> Pcond( e, G );
  @*/
int cond(Te e);

/*@ assigns G;
  @ ensures Body: Pbody( e, \old(G), G );
  @*/
void body(Te e);

/*@ assigns G;
  @ ensures Scond: !Pcond( e, G );
  @ ensures Sloop:  Ploop( e, \old( G ), G );
  @*/
void loop_statement(Te e) {
  /*@ requires Rinv: Pinv(add2_Ts_tail(empty_Ts_list,G), e, G, G);
    @ assigns G;
    @ ensures Scond: !Pcond(e, G );
    @ ensures Sloop:  Ploop(e, \old( G ), G ) ;
    @*/
  /*@ loop assigns G;
    @ loop invariant Iloop: Ploop(e, \at( G, LoopEntry ), G );
    @*/
  while (cond(e))
    /*@ requires Scond: Pcond( e, G );
      @ assigns G ;
      @ ensures Sbody: Pbody( e, \old(G), G );
      @*/
    body(e);
}
