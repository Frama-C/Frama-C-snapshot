/* run.config_qualif
   OPT: -wp-model Typed+ref -wp-prop="-qed_ok,-qed-ko"
   OPT: -wp-model Typed -wp-prop="-qed_ok,-qed-kok"
*/

// Some true properties cannot be proved. config_qualif tests them.
/* -------------------------------------------------------------------------- */
/* --- GOAL: pointer arithmetics handled by each models                   --- */
/* --- TODO: validate tests on other models.                              --- */
/* -------------------------------------------------------------------------- */

int * p ;
int t[10];

/* -------------------------------------------------------------------------- */
/* --- Testing shift from arrays                                          --- */
/* -------------------------------------------------------------------------- */

/*@ ensures Lt: ( \forall integer i, integer j; i < j  ==> t+i <  t+j );
  @ ensures Le: ( \forall integer k, integer l; k <= l ==> t+k <= t+l );
  @ ensures Eq: ( \forall integer m, integer n; m == n ==> t+m == t+n );
*/
void array (void) { return; }

/* -------------------------------------------------------------------------- */
/* --- Testing shift from pointer                                         --- */
/* -------------------------------------------------------------------------- */

/*@ ensures qed_ok: Lt: ( \forall integer i, integer j; i < j  ==> p+i <  p+j );
  @ ensures qed_ok: Le: ( \forall integer k, integer l; k <= l ==> p+k <= p+l );
  @ ensures qed_ok: Eq: ( \forall integer m, integer n; m == n ==> p+m == p+n );
  @ ensures qed_ok: Eq_0:         ( \forall integer n;   n == 0 ==> p == p+n );
  @ ensures qed_ok: Eq_0_bis:     ( \forall integer n; p+n == p ==> n == 0 );
  @ ensures qed_ko: Le_oracle_ko:(\forall integer i, integer j; i <= j ==> p+i <  p+j );
  @ ensures qed_ko: Eq_oracle_ko: ( \forall integer n; p+n == p ==> n == 1 );
*/
void pointer (void) { return; }

/* -------------------------------------------------------------------------- */
/* --- Testing shift from mixed array and pointers                        --- */
/* -------------------------------------------------------------------------- */


/*@  requires \base_addr(p) == \base_addr(&t[0]) ;
    ensures qed_ok: Lt:   p-t >  0 ==> p >  (int *) t ;
    ensures qed_ok: Le:   p-t >= 0 ==> p >= (int *) t ;
    ensures qed_ok: Eq:   p-t == 0 ==> p == (int *) t ;
    ensures qed_ok: Ne:   p-t != 0 ==> p != (int *) t ;
    ensures qed_ko: Le_oracle_ko: p-t >= 0 ==> p >  (int *) t ;
    ensures qed_ko: Lt_oracle_ko: p-t >  0 ==> p <= (int *) t ;
*/
 void mixed_array_pointer (int *p) { return; }

/* -------------------------------------------------------------------------- */
/* --- Testing pointers comparison, base, and minus                       --- */
/* -------------------------------------------------------------------------- */

/*@ requires \base_addr(p) == \base_addr(q) ;
    ensures qed_ok: Lt: p-q >  0 ==> p >  q ;
    ensures qed_ok: Le: p-q >= 0 ==> p >= q ;
    ensures qed_ok: Eq: p-q == 0 ==> p == q ;
 */
void compare (int * q) { return; }

/* -------------------------------------------------------------------------- */
/* --- Testing pointers comparison, base, and minus                       --- */
/* -------------------------------------------------------------------------- */

/*@ requires \base_addr(p) == \base_addr(&t[0]) ;
    ensures qed_ko: Base_oracle_ko: p-q > 0 ==> p > q ; // missing base p == base q !
    ensures qed_ko: Comp_oracle_ko: ( \forall integer i, integer j; i <= j ==> t+i >= t+j );
*/
void absurd (int * q) { return; }

/* -------------------------------------------------------------------------- */
/* --- Testing null                                                       --- */
/* -------------------------------------------------------------------------- */

//TODO: fix problem with null (Cf. translate_expr / translate_prop) with Hoare model.
//      It is ok with store model.

/*@ ensures qed_ok: Bool: \result == (p != \null) ;
    ensures qed_ok: NotNull: p != \null ==> \result != 0 ;
    ensures qed_ok: IsNull:  p == \null ==> \result == 0 ;
*/
int null (int *p) { return (int) p; }
