/* run.config
   OPT: -wp-model +ref
*/

/* run.config_qualif
   OPT: -wp -wp-proof alt-ergo -wp-par 1 -wp-model +ref -wp-timeout 2 -wp-prop qed_ok
   OPT: -wp -wp-proof alt-ergo -wp-par 1 -wp-model +ref -wp-timeout 2 -wp-prop qed_ko
*/

//@ requires qed_ok:\valid(r); ensures qed_ok:*r == 1 ; assigns qed_ok:*r ;
void f(int *r) { *r = 1 ; }

//@ requires qed_ko:\valid(r); ensures qed_ok:*r == 1 ; assigns qed_ok:*r ;
void f_ko(int *r) { *r = 1 ; }

// Pre-condition of f should never hold
//@ ensures qed_ok:\result == 1 ;
int wrong(int * q)
{
  f_ko(q) ;
  return *q ;
}

//@ requires \valid(p) ; ensures qed_ok:\result == 1 ;
int correct(int * p)
{
  f(p) ;
  return *p ;
}

//@ ensures qed_ok:\result == 1 ;
int local()
{
  int u ;
  f(&u) ;
  return u ;
}

//@ ensures qed_ok:\result == 1 ;
int formal(int v)
{
  f(&v) ;
  return v ;
}
