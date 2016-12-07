/* run.config
   OPT:
   OPT: -wp-model +ref
*/

/* run.config_qualif
   OPT:
   OPT: -wp-model +ref
*/

/*@ requires \valid(r);
    ensures *r == 1 ; 
    assigns *r ; */
void f(int *r) { *r = 1 ; }

// Pre-condition of f holds only when q is used ByRef 
/*@ 
  ensures \result == 1 ;
*/
int wrong_without_ref(int * q)
{
  f(q) ;
  return *q ;
}

// Pre-condition of f always holds
/*@ 
  requires \valid(q);
  ensures \result == 1 ;
*/
int pointer(int * q)
{
  f(q) ;
  return *q ;
}

// Pre-condition of f always holds
//@ ensures \result == 1 ;
int local()
{
  int u ;
  f(&u) ;
  return u ;
}

// Pre-condition of f always holds
//@ ensures \result == 1 ;
int formal(int v)
{
  f(&v) ;
  return v ;
}

int g ;

// Pre-condition of f always holds
//@ ensures \result == 1 ;
int global(void)
{
  f(&g) ;
  return g ;
}
