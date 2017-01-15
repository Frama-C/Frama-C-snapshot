//@ghost int calls;
/*@
axiomatic Sequence {
  logic \list<integer> sequence{L} reads calls;
}
*/

#define F 1
#define G 2
#define U 9
#define N (U-1)
#define CALL sequence{Here}
#define CALL_PRE sequence{Pre}

/*@
  ensures CALL == (CALL_PRE ^ [| F |]);
  assigns calls;
*/
void f(void);

/*@
  ensures CALL == (CALL_PRE ^ [| G |]);
  assigns calls;
*/
void g(void);

/*@
  requires CALL_PRE == \Nil;
  ensures CALL == [| F,G,F |];
  assigns calls;
*/
void master(void)
{
  f();
  g();
  f();
}

/*@
  requires CALL_PRE == \Nil;
  ensures CALL == [| F , G |] *^ N ;
  assigns calls;
*/
void unroll(void)
{
  /*@ loop pragma UNROLL "completely", U; */
  for (int i = 0; i < N; i++)
    { f(); g(); }
  return;
}

/*@
  requires 0 <= n ;
  requires CALL_PRE == \Nil;
  ensures CALL == [| F , G |] *^ n ;
  assigns calls;
*/
void induction(int n)
{
  /*@ 
    loop invariant 0 <= i <= n;
    loop invariant CALL == [| F , G |] *^ i;
    loop assigns i,calls; 
   */
  for (int i = 0; i < n; i++)
    { f(); g(); }
  return;
}

/*@
  requires 0 <= n ;
  requires CALL_PRE == \Nil;
  ensures CALL == [| F , G |] *^ (n+1) ;
  assigns calls;
*/
void shifted(int n)
{
  f();
  
  /*@ 
    loop invariant 0 <= i <= n;
    loop invariant CALL == ([| F , G |] *^ i ^ [| F |]);
    loop assigns i,calls; 
   */
  for (int i = 0; i < n; i++)
    { g(); f(); }

  g();
  return;
}
