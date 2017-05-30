/* run.config
   OPT: -wp-model Caveat
 */

/* run.config_qualif
   OPT: -wp -wp-model Caveat -wp-prover alt-ergo -wp-depth 16 -wp-prop="-ko"
   OPT: -wp -wp-model Caveat -wp-prover why3:alt-ergo -wp-depth 16 -wp-prop="-ko,-bug_why3"
   OPT: -wp -wp-model Caveat -wp-prover alt-ergo -wp-depth 16 -wp-prop="ko" -wp-timeout 2
*/

//@ ghost int call_seq;
/*@ axiomatic Call {
  @   logic \list<integer> call_obs{L} reads call_seq ;
  @   logic \list<integer> call_nil = [| |];
  @
  @ }
  @*/

/*@
  assigns call_seq;
  ensures call_obs == (\old(call_obs) ^ [| a |]);
*/
void f(int a);

/*@
  assigns call_seq;
  ensures call_obs == (\old(call_obs) ^ [| b |]);
*/
void g(int b);

//--- no calls -----------------------------------------

/*@
  requires init: call_obs == \Nil;
  assigns call_seq;
  ensures ok: m1: \length (call_obs) == 0;
  ensures ok: m2: \length (call_obs) == \length (call_nil);
  ensures ok: bug_why3: n1: call_obs == \old(call_obs);
  ensures ok: n2: call_obs == call_nil;
  ensures ok: n3: call_obs == (call_nil ^ \old(call_obs) ^ \Nil);
  //ensures ok: n4: call_obs == (\Nil ^ \old(call_obs) ^ call_nil);
  ensures ok: bug_why3: n5: 0<=a ==> call_obs == (call_nil *^ a);
  ensures ok: bug_why3: n6: 0<=a ==> call_obs == (\old(call_obs) *^ a);
  ensures ok: bug_why3: n5_ok: call_obs == (call_nil *^ a);
  ensures ok: bug_why3: n6_ok: call_obs == (\old(call_obs) *^ a);
 */
void no_calls(int a)
{ ;
}

//--- sequential call  ---------------------------------
/*@
  requires call_obs == \Nil;
  assigns call_seq;
  behavior g_called:
    assumes c!=0;
    ensures ok: o1: \length (call_obs) == 3;
    ensures ok: p1: call_obs == (\old(call_obs) ^ [| x, y, z |]);
    ensures ok: p2: call_obs == (\old(call_obs) ^ [| x |] ^ [| y |] ^ [| z |] ^ \Nil);
    ensures ok: p3: call_obs == (\old(call_obs) ^ [| x |] ^ \Nil ^ [| y |] ^ [| z |] ^ call_nil);
  behavior g_not_called:
    assumes c==0;
    ensures ok: o2: \length (call_obs) == 2;
    ensures ok: q1: call_obs == (\old(call_obs) ^ [| x, z |]);
    ensures ok: q2: call_obs == (\old(call_obs) ^ [| x |] ^ ([| y |] *^ c) ^ [| z |] ^ \Nil);
    ensures ok: q3: call_obs == (\old(call_obs) ^ [| x |] ^ call_nil ^ [| z |] ^ \Nil);
*/
void sequence(int c, int x, int y, int z)
{
  f(x);
  if (c)
    g(y);
  f(z);
}

//--- sequential call  ---------------------------------
/*@
  requires call_obs == \Nil;
  assigns call_seq;
  ensures ok: first: \nth(call_obs,0)==x;
  ensures ok: last:  \nth(call_obs,\length(call_obs)-1)==z;
  behavior g_called:
    assumes n>0;
    ensures ok: u1: \length(call_obs) == 2 + n;
    ensures ok: u2: call_obs == (\old(call_obs) ^ [| x |] ^ ([| y |] *^ n) ^ [| z |]);
  behavior g_not_called:
    assumes n<=0;
    ensures ok: v1: \length(call_obs) == 2;
    ensures ok: v2: call_obs == (\old(call_obs) ^ [| x, z |]);
*/
void loops(int n, int x, int y, int z)
{
  int i;
  f(x);
  /*@ loop assigns i, call_seq;
      loop invariant ok: id_min: 0 <= i;
      loop invariant ok: id_max: (0 <= n ? i <= n : i <= 0);
      loop invariant ok: inv: call_obs == (\at(call_obs,LoopEntry) ^ ([| y |] *^ i)) ;
  */
  for (i=0; i<n; i++)
    g(y);
  f(z);
}

//--- end ------------ ---------------------------------
