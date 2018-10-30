/* run.config
   CMD: @frama-c@ -wp -wp-prover none -wp-check -wp-share ./share -wp-msg-key shell -wp-msg-key rte
   OPT: -wp-rte -warn-invalid-bool -wp-bool-range -then -print -no-unicode
   OPT: -wp-rte -no-warn-signed-overflow -then -print -no-unicode
   OPT: -wp-rte -warn-unsigned-overflow -then -print -no-unicode
   OPT: -wp-rte -wp-model +nat -then -print -no-unicode
   OPT: -wp-rte -wp-model +nat -warn-unsigned-overflow -then -print -no-unicode
   OPT: -wp-no-rte -wp-model +nat
   OPT: -wp-rte -rte-no-mem -wp-model +nat
 */
/* run.config_qualif
   OPT: -wp-rte -warn-invalid-bool -wp-bool-range -wp-prop=rte
 */


//@ axiomatic Obs { predicate R(integer r); }

//@ ensures R(\result);
int job(int *p,int n)
{
  return (*p += n) ;
}

//@ ensures R(\result);
unsigned job2(unsigned a,unsigned b)
{
  return a+b;
}

_Bool X;
//@ ensures R(\result);
int job3(void)
{
  return X ;
}
