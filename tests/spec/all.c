/* Terms */

/*@ lemma z: \forall int x ; (x & x) == x ; */          // OK
/*@ lemma a: \forall int x ; (x --> x) == -1 ; */       // OK
/*@ lemma b: \forall int x ; (x <--> x) == -1 ; */      // OK
/*@ lemma c: (\let x = 0 ; x+1) == 1 ; */               // OK
/*@ lemma d: (name:77) == 76+1 ; */                     // OK

/* Predicates */
/*@
  axiomatic Test {
    predicate P ;                                     // OK
    predicate Q ;                                     // OK

    axiom e: P ^^ Q ;                                 // OK
    axiom f: 0?P:Q;                                   // OK
    axiom g: P?P:Q;                                   // OK
    axiom h: \let x = 0 ; x+1 == 1 ;                  // OK
    axiom i: name:77 == 76+1 ;                        // OK
}
*/

/*@ predicate R(integer i, integer j) =
             (1?i+j:j:j)==i+j;*/                         // OK
/*@ predicate S(integer i, integer j) =
           (1?(i:j):j)==j;  */                           // OK
/*@ predicate T(integer i, integer j) =
           (1?i:j)==i;  */                               // OK
/*@ lemma tauto: 0?T(0,0):R(1,2); */                     // OK
/*@ lemma tauto2: R(0,1)?S(3,4):T(5,6); */               // OK

/*@ lemma reject_1 : 0 != 1 != 2 ; */                    // OK

/*@ lemma hex_oct : 0xFFFFUl != 06666uL ; */             // OK

/*@
  requires \offset(p) == 0;                              // OK
  behavior b :
   assumes \true;
   requires \valid(p);
   ensures 0 == 1 ;
   assigns *p \from G ;
*/                                                       // OK
void h(int G,int*p) ;
/*@
  behavior b :
   assumes \true;
   requires \valid(p);
   ensures 0 == 1 ;
   assigns *p \from G = G + 77;
*/                                                       // KO (functional update
void f(int G,int*p) {

  //@ for ZZZ_INEXISTENT_BEHAVIOR : assert \false ;      // OK
 //@ assert \false ;                                     // OK
  /*@ assert \base_addr(&G) == \base_addr(&G) ; */       // OK
  /*@ assert \block_length(&G) == 4 ; */                 // OK
  /*@ assert \block_length(&G) == sizeof(G) ; */         // OK
  /*@ assert
      \base_addr(&G)+\offset(&G+4) == (char*)(&G+4); */  // OK
  /*@ assert \null != &G ; */                            // OK
  /*@ loop invariant &G != \null; */                     // OK
  do
    G++;
  while (0) ;

  *p = G + 76;
}

struct st { int a, b ; } ;
/*@ axiomatic St { logic struct st fl(struct st s) ; } */ //OK
/*@ ensures fl(s).a == \result.a ; */ // OK
struct st fc (struct st s) {return s;}

void fd(char *x) { 
  /*@ assert (const char*)x == (char * const) x; */
  x="abcdef";
  //@ assert !\valid(x) && \valid_read(x);                 // OK
  return;
}

/*@ ensures \result==0; */ // should be rejected: not a function
int x = 0;

/*@ logic integer x = 1 ; */                              // OK
/*@ axiomatic Test2 {logic integer y ;} */                // OK
/*@ logic integer z = \let f = \lambda integer a ; a + 1; f(12) ; */ // OK
/*@ logic a id<a>(a x) = x;   */                          // OK
/*@ logic integer z1 = \max(5,10,id) ; */                  // OK
/*@ logic integer z2 = \min(5,10,id) ; */                  // OK
/*@ logic integer z3 = \sum(5,10,id) ; */                  // OK
/*@ logic integer z4 = \product(5,10,id) ; */              // OK
/*@ logic integer z5 = \numof(0,10,\lambda integer i; 3<=i<=5) ; */ // OK

/* ALL CONCRETE LOGIC TYPES */
