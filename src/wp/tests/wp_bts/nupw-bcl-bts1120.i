/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-fct "g,unreachable_smt_with_contract"
*/

/*@ axiomatic ax {
  @ predicate ExitF(integer x);
  @ predicate ExitP(integer x);
  @ predicate Exit1(integer x);

  @ predicate PostF(integer x);
  @ predicate PostP(integer x);
  @ predicate Post1(integer x);

  @ predicate P(integer x);

  @ predicate PreF(integer x);
  @ predicate Pre(integer x);
  @ predicate Pre1(integer x);
} */

//@ assigns \nothing; ensures PostF(x); exits ExitF(x) ;
int f(int x);

// corrected.
//@ requires ExitF(max) ==> ExitP(max); assigns \nothing; exits ok:ExitP(max);
void g (int max) {
  int tmp = f(max);
  //@ loop assigns ok:tmp;
  while (tmp<=max) {
     tmp ++;
  }
}

//@ requires ok: x > 0 ; assigns \nothing;
extern int f_with_precond (int x);

// corrected.
//@ requires PostP(max); ensures ok: PostP(max);
void unreachable_smt_with_contract (int max) {
  int tmp = f_with_precond(1);
  goto L;
  //@ requires ok: Pre1(max); assigns ok: tmp; ensures ok: Post1(max); exits ok: Exit1(max);
  tmp = f_with_precond(-2);
  //@ assert ok: P(tmp); 
  tmp=3;
  L:;
}

//@ assigns \nothing; exits never: \false;
int f_no_exit(int) ;

// corrected in stronger the PO (e1 is forgotten and is not provable *)
//@ exits e:ExitP(0);
int cfg_domination_problem (int max) {
  int tmp=1;
  if (max) {
    tmp=f_no_exit(tmp);
    goto L;
  }
  //@ assigns tmp; exits e1:ExitP(max);
  {
    tmp=f(max);
 L: tmp=3;
  }
  return tmp;
}
