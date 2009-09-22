/* run.config
   DONTRUN: all of them fail at this time!
*/

/* Terms */

/*@ axiom a: \forall int x ; x --> x == -1 ; */          // KO
/*@ axiom b: \forall int x ; x <--> x == -1 ; */         // KO
/*@ axiom c: (\let x = 0 ; x+1) == 1 ; */                // KO
/*@ axiom d: (name:77) == 76+1 ; */                      // OK

/* Predicates */
/*@ predicate P ; */                                     // OK
/*@ predicate Q ; */                                     // OK

/*@ axiom e: P ^^ Q ; */                                 // OK
/*@ axiom f: 0?P:Q; */                                   // OK
/*@ axiom g: P?P:Q; */                                   // KO
/*@ axiom h: \let x = 0 ; x+1 == 1 ; */                  // KO
/*@ axiom i: name:77 == 76+1 ; */                        // OK

/*@ predicate R(integer i, integer j) =
             (1?i+j:j:j)==i+j;*/                         // OK
/*@ predicate S(integer i, integer j) =
           (1?(i:j):j)==j;  */                           // OK
/*@ predicate T(integer i, integer j) =
           (1?i:j)==i;  */                               // OK
/*@ axiom tauto: 0?T(0,0):R(1,2); */                     // OK
/*@ axiom tauto2: R(0,1)?S(3,4):T(5,6); */               // KO

/*@ axiom reject_1 : 0 != 1 != 2 ; */                    // OK

/*@ axiom hex_oct : 0xFFFFUl != 06666uL ; */             // OK

/*@
  behavior b :
   requires \valid(p);
   assumes \true;
   ensures 0 == 1 ;
   assigns *p \from G = G + 77;
*/                                                       // KO
void f(int G,int*p) {

  //@ for b : assert \true ;                             // KO
 //@ assert \true ;                                      // OK
  /*@ assert \base_addr(&G) == \base_addr(&G) ; */       // OK
  /*@ assert \block_length(&G) == 4 ; */                 // OK
  /*@ assert \block_length(&G) == sizeof(G) ; */         // OK
  /*@ assert
      \base_addr(&G) + \offset(&G+4) == &G + 4 ; */      // KO
  /*@ assert \null != &G ; */                            // OK
  do
    G++;
  /* loop invariant G >= \null */                        // KO
  while (0) ;

  *p = G + 76;
}

struct st { int a, b ; } ;
/*@ axiomatic St { logic struct st fl(struct st s) ; } */
/*@ ensures fl(s).a == \result.a ; */
struct st fc (struct st s) ;

/*@ logic integer x = 1 ; */                              // OK
/*@ logic integer y ; */                                  // OK
/*@ logic integer z = (\lambda integer a ; a + 1) 12 ; */ // KO
/*@ logic a id<a>(a x) { x }  */                          // OK
/*@ logic integer z = \max(5,10,id) ; */                  // KO
/*@ logic integer z = \min(5,10,id) ; */                  // KO
/*@ logic integer z = \sum(5,10,id) ; */                  // KO
/*@ logic integer z = \product(5,10,id) ; */              // KO
/*@ logic integer z = \numof(0,10,id) ; */                // KO

/* ALL CONCRETE LOGIC TYPES */
