/* run.config
   OPT: -wp-dynamic -wp-msg-key "calls"
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
int X1;
//@ assigns X1; ensures X1==1;
int h1(void);

int X2;
//@ assigns X2; ensures X2==2;
int h2(void);

//@ assigns \nothing;
int h0(void);

/*@ behavior bhv1:
  @   assumes p == &h1;
  @   assigns X1;
  @   ensures X1==1; */
int behavior (int (*p)(void)) {
  //@ calls h1, h2; // Shall not be proved in default behavior (known bug)
  return (*p)();
}

/*@ behavior bhv1:
  @   assumes p == &h1;
  @   assigns X1;
  @   ensures X1==1;
  @ behavior bhv0:
  @   assumes p == &h0;
  @   assigns \nothing;
  @   ensures X1==\old(X1); */
int some_behaviors (int (*p)(void)) {
  //@ for bhv1,bhv0: calls h1, h2, h0;
  return (*p)();
}

/*@
  ensures X1==1;
  assigns X1 ;
*/
int missing_context (int (*p)(void)) {
  //@ calls h1 ;
  return (*p)();
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
