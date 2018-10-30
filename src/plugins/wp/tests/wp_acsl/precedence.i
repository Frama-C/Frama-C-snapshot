/* run.config
   OPT: -kernel-warn-key=annot-error=active -print
*/
/* run.config_qualif
   OPT: -kernel-warn-key=annot-error=active -wp -wp-model Typed -wp-par 1 -wp-prop="-ko"
   OPT: -kernel-warn-key=annot-error=active -wp -wp-model Typed -wp-par 1 -wp-prop="ko" -wp-steps 50
*/

/* Test of operator precedence and associativity.
 * - The option -kernel-warn-key annot-error=inactive allow to skip contracts that contain a 'badly_formed' term.
 *   Since the whole contract is rejected in a such case, these contracts must content only one property.
 *   So, they are put inside a statement contract. 
 */

//@ axiomatic Pred  { predicate P; predicate Q; predicate R; predicate S; predicate U;}
//@ axiomatic PredX { predicate Px(integer x); predicate Qx(integer x); predicate Rx(integer x);}
//@ axiomatic PredXY{ predicate Pxy(integer x, integer y); predicate Qxy(integer x, integer y);}

/*@  // PREDICATE OPERATORS ------------------------------------------------
  @ ensures     r_precedence_and_xor: (P && Q ^^ R) <==> ((P && Q) ^^ R);
  @ ensures     l_precedence_and_xor: (P ^^ Q && R) <==> (P ^^ (Q && R));
  @ ensures ko: l_precedence_xor_and: (P && Q ^^ R) <==> (P && (Q ^^ R));
  @ ensures ko: r_precedence_xor_and: (P ^^ Q && R) <==> ((P ^^ Q) && R);

  @ ensures     r_precedence_xor_or: (P ^^ Q || R) <==> ((P ^^ Q) || R);
  @ ensures     l_precedence_xor_or: (P || Q ^^ R) <==> (P || (Q ^^ R));
  @ ensures ko: l_precedence_or_xor: (P ^^ Q || R) <==> (P ^^ (Q || R));
  @ ensures ko: r_precedence_or_xor: (P || Q ^^ R) <==> ((P || Q) ^^ R);

  @ ensures     r_precedence_or_implies: (P || Q ==> R) <==> ((P || Q) ==> R);
  @ ensures     l_precedence_or_implies: (P ==> Q || R) <==> (P ==> (Q || R));
  @ ensures ko: l_precedence_implies_or: (P || Q ==> R) <==> (P || (Q ==> R));
  @ ensures ok: r_precedence_implies_or: (P ==> Q || R) <==> ((P ==> Q) || R);
  @ ensures ok: since:                 ((P ==> Q) || R) <==> ((P ==> Q) || R);

  @ ensures     r_assoc_implies: (P ==> Q ==> R) <==> (P ==> (Q ==>R));
  @ ensures ko: l_assoc_implies: (P ==> Q ==> R) <==> ((P ==> Q) ==>R);

  @ ensures     r_precedence_implies_equiv: (P ==> Q <==> R) <==> ((P ==> Q) <==> R);
  @ ensures     l_precedence_implies_equiv: (P <==> Q ==> R) <==> (P <==> (Q ==> R));
  @ ensures ko: r_precedence_equiv_implies: (P ==> Q <==> R) <==> (P ==> (Q <==> R));
  @ ensures ko: l_precedence_equiv_implies: (P <==> Q ==> R) <==> ((P <==> Q) ==> R);

  @ ensures     r_precedence_equiv_ite: (P <==> Q ? R : S) <==> ((P <==> Q) ? R : S);
  @ ensures     m_precedence_equiv_ite: (P ? Q <==> R : S) <==> (P ? (Q <==> R) : S);
  @ ensures     l_precedence_equiv_ite: (P ? Q : R <==> S) <==> (P ? Q : (R <==> S));
  @ ensures ko: r_precedence_ite_equiv: (P <==> Q ? R : S) <==> (P <==> (Q ? R : S));
  @ ensures ko: l_precedence_ite_equiv: (P ? Q : R <==> S) <==> ((P ? Q : R) <==> S);
  
  @ ensures     r_assoc_ite: (P ? Q : R ? S : U) <==> (P ? Q : (R ? S : U));
  @ ensures ko: l_assoc_ite: (P ? Q : R ? S : U) <==> ((P ? Q : R) ? S : U);

  @ ensures     r_precedence_ite_forall: (\forall integer x; Px(x) ? Qx(x) : Rx(x)) <==> (\forall integer y; (Px(y) ? Qx(y) : Rx(y)));
  @ ensures     m_precedence_ite_forall: (Px(x) ? \forall integer x; Qx(x) : Rx(x)) <==> (Px(x) ? (\forall integer y; Qx(y)) : Rx(x));
  @ ensures     l_precedence_ite_forall: (Px(x) ? Qx(x) : \forall integer x; Rx(x)) <==> (Px(x) ? Qx(x) : (\forall integer y; Rx(y)));
  @ ensures ko: r_precedence_forall_ite: (\forall integer x; Px(x) ? Qx(x) : Rx(x)) <==> ((\forall integer y; Px(y)) ? Qx(x) : Rx(x));
  @ ensures ko: m_precedence_forall_ite: (Px(x) ? \forall integer x; Qx(x) : Rx(x)) <==> (Px(x) ? (\forall integer y; Qx(x)) : Rx(x));
  @ ensures ko: l_precedence_forall_ite: (Px(x) ? Qx(x) : \forall integer x; Rx(x)) <==> (Px(x) ? Qx(x) : (\forall integer y; Rx(x)));

  @ ensures     r_assoc_forall: (\forall integer x; \forall integer x ; Pxy(x, x)) <==> (\forall integer x ; Pxy(x, x));
  @ ensures ko: r_assoc_forall: (\forall integer x; \forall integer y ; Pxy(x, y)) <==> (\forall integer x ; Pxy(x, x));

  @ ensures     r_precedence_ite_exists: (\exists integer x; Px(x) ? Qx(x) : Rx(x)) <==> (\exists integer y; (Px(y) ? Qx(y) : Rx(y)));
  @ ensures     m_precedence_ite_exists: (Px(x) ? \exists integer x; Qx(x) : Rx(x)) <==> (Px(x) ? (\exists integer y; Qx(y)) : Rx(x));
  @ ensures     l_precedence_ite_exists: (Px(x) ? Qx(x) : \exists integer x; Rx(x)) <==> (Px(x) ? Qx(x) : (\exists integer y; Rx(y)));
  @ ensures ko: r_precedence_exists_ite: (\exists integer x; Px(x) ? Qx(x) : Rx(x)) <==> ((\exists integer y; Px(y)) ? Qx(x) : Rx(x));
  @ ensures ko: m_precedence_exists_ite: (Px(x) ? \exists integer x; Qx(x) : Rx(x)) <==> (Px(x) ? (\exists integer y; Qx(x)) : Rx(x));
  @ ensures ko: l_precedence_exists_ite: (Px(x) ? Qx(x) : \exists integer x; Rx(x)) <==> (Px(x) ? Qx(x) : (\exists integer y; Rx(x)));

  @ ensures     r_assoc_exist: (\exists integer x; \exists integer x ; Pxy(x, x)) <==> (\exists integer x ; Pxy(x, x));
  @ ensures ko: r_assoc_exist: (\exists integer x; \exists integer y ; Pxy(x, y)) <==> (\exists integer x ; Pxy(x, x));

  @ ensures     r_precedence_ite_let: (\let x=a; Px(x) ? Qx(x) : Rx(x)) <==> (\let y=a; (Px(y) ? Qx(y) : Rx(y)));
  @ ensures     m_precedence_ite_let: (Px(x) ? \let x=a; Qx(x) : Rx(x)) <==> (Px(x) ? (\let y=a; Qx(y)) : Rx(x));
  @ ensures     l_precedence_ite_let: (Px(x) ? Qx(x) : \let x=a; Rx(x)) <==> (Px(x) ? Qx(x) : (\let y=a; Rx(y)));
  @ ensures ko: r_precedence_let_ite: (\let x=a; Px(x) ? Qx(x) : Rx(x)) <==> ((\let y=a; Px(y)) ? Qx(x) : Rx(x));
  @ ensures ko: m_precedence_let_ite: (Px(x) ? \let x=a; Qx(x) : Rx(x)) <==> (Px(x) ? (\let y=a; Qx(x)) : Rx(x));
  @ ensures ko: l_precedence_let_ite: (Px(x) ? Qx(x) : \let x=a; Rx(x)) <==> (Px(x) ? Qx(x) : (\let y=a; Rx(x)));

  @ ensures     r_assoc_let: (\let x=a; \let x=b ;   Pxy(x, x)) <==> (\let x=b ; Pxy(x, x));
  @ ensures     scope_let:   (\let x=a; \let x=x+1 ; Px(x))     <==> Px(a+1);
  @ ensures     scope_let:   (\let x=x; \let y=x+1 ; Pxy(x, y)) <==> Pxy(x, x+1);

  @ ensures ko: l_assoc_naming:          (P ? Q : R : S) <==> (P ? (Q : R) : S);
  @ ensures     r_precedence_ite_naming: (P ? Q : R : S) <==> (P ?  Q : (R: S));
  
*/
void predicate(int x, int a, int b) { 
  // Properties that have to be rejected at the parsing.
  //@ ensures badly_formed: ;
  ;
}

/*@ // COMPARISON OPERATORS ------------------------------------------------
  @ ensures      chainable_lt_lt: (p <  q <  r) <==> ((p <  q) && (q <  r));
  @ ensures      chainable_le_le: (p <= q <= r) <==> ((p <= q) && (q <= r));
  @ ensures      chainable_gt_gt: (p >  q >  r) <==> ((p >  q) && (q >  r));
  @ ensures      chainable_ge_ge: (p >= q >= r) <==> ((p >= q) && (q >= r));
  @ ensures      chainable_eq_eq: (p == q == r) <==> ((p == q) && (q == r));

  @ ensures     r_precedence_eq_and: (p == q && R) <==> ((p == q) && R);
  @ ensures     l_precedence_eq_and: (P && q == r) <==> (P && (q == r));
  @ ensures ko: r_precedence_and_eq: (p == q && r) <==> (p == (q && r));
  @ ensures ko: l_precedence_and_eq: (p && q == r) <==> ((p && q) == r);

  @ ensures ko: l_nonassoc_eq: (p == q == r) <==> ((p == q) == r);
  @ ensures ko: r_nonassoc_eq: (p == q == r) <==> (p == (q == r));

  @ ensures     r_precedence_neq_and: (p != q && R) <==> ((p != q) && R);
  @ ensures     l_precedence_neq_and: (P && q != r) <==> (P && (q != r));
  @ ensures ko: r_precedence_and_neq: (p != q && r) <==> (p != (q && r));
  @ ensures ko: l_precedence_and_neq: (p && q != r) <==> ((p && q) != r);

*/
void comparison(int p, int q, int r) {
  // Properties that have to be rejected at the parsing:

  //@ ensures badly_formed: unchainable_eq_ne: (p == q != r) <==> ((p == q) && (q != r));
  //@ ensures badly_formed: unchainable_ne_eq: (p != q == r) <==> ((p != q) && (q == r));
  //@ ensures badly_formed: unchainable_ne_ne: p != q != r;

  //@ ensures badly_formed: unchainable_lt_ne: p <  q != r;
  //@ ensures badly_formed: unchainable_le_ne: p <= q != r;
  //@ ensures badly_formed: unchainable_gt_ne: p >  q != r;
  //@ ensures badly_formed: unchainable_ge_ne: p >= q != r;

  //@ ensures badly_formed: unchainable_ne_lt: p != q <  r;
  //@ ensures badly_formed: unchainable_ne_le: p != q <= r;
  //@ ensures badly_formed: unchainable_ne_gt: p != q >  r;
  //@ ensures badly_formed: unchainable_ne_ge: p != q >= r;

  //@ ensures badly_formed: unchainable_lt_gt: p <  q >  r;
  //@ ensures badly_formed: unchainable_lt_ge: p <  q >= r;
  //@ ensures badly_formed: unchainable_le_gt: p <= q >  r;
  //@ ensures badly_formed: unchainable_le_ge: p <= q >= r;
 ; 
}

/*@ // BITWISE OPERATORS ------------------------------------------------
  @ ensures     r_precedence_and_xor: (p & q ^ r) == ((p & q) ^ r);
  @ ensures     l_precedence_and_xor: (p ^ q & r) == (p ^ (q & r));
  @ ensures ko: l_precedence_xor_and: (p & q ^ r) == (p & (q ^ r));
  @ ensures ko: r_precedence_xor_and: (p ^ q & r) == ((p ^ q) & r);

  @ ensures     r_precedence_xor_or: (p ^ q | r) == ((p ^ q) | r);
  @ ensures     l_precedence_xor_or: (p | q ^ r) == (p | (q ^ r));
  @ ensures ko: l_precedence_or_xor: (p ^ q | r) == (p ^ (q | r));
  @ ensures ko: r_precedence_or_xor: (p | q ^ r) == ((p | q) ^ r);

  @ ensures     r_precedence_or_implies: (p | q --> r) == ((p | q) --> r);
  @ ensures     l_precedence_or_implies: (p --> q | r) == (p --> (q | r));
  @ ensures ko: l_precedence_implies_or: (p | q --> r) == (p | (q --> r));
  @ ensures ok: r_precedence_implies_or: (p --> q | r) == ((p --> q) | r);
  @ ensures ok: since:                 ((p --> q) | r) == ((p --> q) | r);

  @ ensures     r_assoc_implies: (p --> q --> r) == (p --> (q -->r));
  @ ensures ko: l_assoc_implies: (p --> q --> r) == ((p --> q) -->r);

  @ ensures     r_precedence_implies_equiv: (p --> q <--> r) == ((p --> q) <--> r);
  @ ensures     l_precedence_implies_equiv: (p <--> q --> r) == (p <--> (q --> r));
  @ ensures ko: r_precedence_equiv_implies: (p --> q <--> r) == (p --> (q <--> r));
  @ ensures ko: l_precedence_equiv_implies: (p <--> q --> r) == ((p <--> q) --> r);
*/
void bitwise(int p, int q, int r) {
  // Properties that have to be rejected at the parsing:
  ;
}

/*@ // MIXES PREDICATE AND BITWISE OPERATORS ------------------------------------------------
  @ ensures     r_precedence_equiv_Pand: (p <--> q && R) <==> ((p <--> q) && R);
  @ ensures     l_precedence_equiv_Pand: (P && q <--> r) <==> (P && (q <--> r));
*/
void predicate_bitwise(int p, int q, int r) {
  // Properties that have to be rejected at the parsing:
  //@ ensures ko: badly_formed: l_precedence_Pand_equiv: (p <--> q && R) <==> (p <--> (q && R));
  //@ ensures ko: badly_formed: r_precedence_Pand_equiv: (P && q <--> r) <==> ((P && q) <--> r);
  ;
}
