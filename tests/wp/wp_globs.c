/*@ logic boolean ok0 (integer i) = (0<= i)  ; 
  @ logic boolean ok1 (integer i) = (i <= 10)  ;
  @ logic boolean ok (integer i) = ok0 (i) && ok1 (i) ;
  */

//@ predicate is_ok (int i) = (0 <= i && i <= 10) ;

/* TODO : test ok with GUI but incorrect interpretation of alt-erdo results
 * with why-dp...
 */

/*@ behavior b_logic : ensures ok (\result);
  @ behavior b_predicate : ensures is_ok (\result);
*/
int test (int x) {
  if (x < 0) return 0;
  //@ for b_logic : assert ok0 (x);
  if (10 < x) return 10;
  //@ for b_logic : assert ok1 (x);
  return x;
}

//WARNING : predicate pos_at{L} (integer n) = (0<= n)  ;
// doesn't means : 0 <= \at(n, L) ! 
// because 'n' is a value; it is not related to a memory state...

//@ predicate positive (integer n) = (0 < n) ;

/*@ ensures positive (x) ==> positive (\result); 
 */
int labpred (int x) {
  return x+1;
}

int G;

//@ predicate gtG_at{L} (integer n) = (G < n)  ;

/*@ ensures gtG_at{Old}(x) ==> gtG_at (\result); 
 */
int labpredGat (int x) {
  G--;
  return x-1;
}

//@ predicate Gincr{L1,L2} = (\at(G, L1) < \at(G, L2))  ;

/*@ requires positive(x);
  @ ensures Gincr{Old,Here};
 */
void labpred2 (int x) {
  G += x;
}


#if 0
/*@ axiomatic A1 {
      predicate ok_with_G{L}(integer n) reads G ; 
      axiom gt_ok{L} : 
        \forall integer x; x > \at(G,L) ==> ok_with_G{L}(x);
    }
*/
/*@ axiomatic A2 {*/
      predicate biggerG{L1, L2}(integer n) reads G ; 
      axiom ax_biggerG {L1,L2} : 
        \forall integer x; \at(G,L1) + x <= \at(G,L2) ==> biggerG{L1,L2}(x);
	/*
    }
*/
#endif

int T [10];

//@ predicate Tn{LT, LG}(integer i) = \at(T[\at(G,LG)], LT) > i;

void testTn () {
  T[G] = 1;
  G ++;
  //@ assert Tn{Here,Pre}(0);
}
//------------------------------------------------------------------------------
// Logic functions + axioms with label
//@ logic boolean eq_ptr{L} (int * p, integer x) = *p == x;

/*@ requires \valid (p); 
    behavior test: assumes eq_ptr (p, 3); ensures \result == 3;
*/
int read_p (int * p) {
  return *p;
}
//------------------------------------------------------------------------------
/*@ axiomatic Cpt {
  @
  @ logic integer cpt_le_u{L} (int *t, integer u, integer n, integer m);
  @
  @ axiom c1{L} : \forall int *t, integer u, integer i;
  @   i >= 0 && u >= t[i] ==>
  @     cpt_le_u{L}(t, u, 0, i) == cpt_le_u{L}(t, u, 0, i-1) + 1;
  @
  @ axiom c2{L} : \forall int *t, integer u, integer i;
  @   i >= 0 && u < t[i] ==>
  @     cpt_le_u{L}(t, u, 0, i) == cpt_le_u{L}(t, u, 0, i-1);
  @
  @ axiom c3{L} : \forall int *t, integer u, integer i;
  @   i < 0 ==> cpt_le_u{L}(t, u, 0, i) == 0;
  @ }
  @*/

//@ ensures \result == cpt_le_u{Here}(t, M, 0, 9);
int cpt (int * t, int M)
{
  int s = 0;

/*@ loop invariant 0 <= i && i <= 10;
    loop invariant I2: s == cpt_le_u{Here}(t, M, 0, i-1);
    loop assigns i, s;
*/
  for (int i = 0; i < 10; i++) {
    if (t[i] <= M)
      s++;
    }
  return s;
}
//------------------------------------------------------------------------------
/*@ axiomatic Le { 
  @ logic boolean le_G{L} (int x) reads G;
  @ }
*/

void logic_with_reads  (int x) {
  G = 0;
  //@ assert ko: le_G{Here}(x) == le_G{Pre}(x);
}


//------------------------------------------------------------------------------
int main (void) { return 0 ; }
//------------------------------------------------------------------------------
