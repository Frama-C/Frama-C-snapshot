/* run.config
   OPT: -wp-dynamic
*/

/* run.config_qualif
   OPT: -wp-dynamic -wp
*/
//-----------------------------------------------------------------------------
/*@ 
  requires -10<=x<=10;
  ensures \result == x+1; 
  assigns \nothing; 
*/
int f1(int x);

/*@ ensures \result == x-1; assigns \nothing; */
int f2(int x);

typedef struct S {
  int param ;
  int (*f)(int) ;
} ;

/*@
  requires (closure->f == &f1 && \abs(closure->param)<=5) || closure->f == &f2 ; 
  ensures \abs(\result - closure->param) <= 1 ;
 */
int call(struct S * closure) {
  /*@ calls f1,f2 ; */
  return (closure -> f)(closure -> param) ;
}
//-----------------------------------------------------------------------------
int X;
//@ assigns X; ensures X==x;
int g(int x);

/*@ requires p->f == &g || p->f == (int (*)(int)) 0;
  @ ensures p->f == &g               ==> X==1;
  @ ensures p->f == (int (*)(int)) 0 ==> X==\old(X);
 */
void guarded_call (struct S * p) {
  if (p->f != (int (*)(int)) 0)
    //@ calls g;
    (* (p->f))(1);
}

//-----------------------------------------------------------------------------
//@ requires \false; ensures \false; exits \false; assigns \nothing;
int unreachable_g(int x);

//@ ensures X==\old(X);
void no_call (void) {
  struct S * p;
  p->f = (int (*)(int)) 0;
  if (p->f != (int (*)(int)) 0)
    //@ calls unreachable_g;
    (* (p->f))(1);
}
//-----------------------------------------------------------------------------
