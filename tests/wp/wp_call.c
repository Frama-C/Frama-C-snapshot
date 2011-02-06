/* run.config_phoare
  OPT:  -journal-disable -rte -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
  OPT:  -rte -journal-disable -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
*/

struct Ts { int a; int b; };
int G;
struct Ts S;
int * P;
int T[10];
int X;

//------------------------------------------------------------------------------
/*@ 
  requires \true;
  assigns G, S.b;
  ensures G == \old(G) + x + 1
       && S.b == \old(S.b) + 1
       &&  \result == x + 1;
*/
int add_G (int x) {
  x++;
  G += x ;
  S.b++;
  return x;
}

/*@ 
  requires G == 0;
  ensures G == 6 && S.a == 1 && S.b == 1 && \result == 6;
*/
int main(void) {
  int r;

  S.a = 1;
  S.b = 0;

  r = add_G (5);
  
  return r;
}
//------------------------------------------------------------------------------

// this is to test the assigns order : T[X] is T[\old(X)] !
//@ assigns T[X], T[X+1], X;
void f (int x) {
  T[X] = 0;
  T[X+1] = 0;
  X = x;
}

// Be carreful : wrong translation of quantification in M2
//@ ensures (\forall int i; 2 <= i < 10 ==> T[i] == \old(T[i]));
void call_f (void) {
  int a = 3;
  X = 0;
  f (a);
}

/*@ ensures ko0: (T[0] == \old(T[0])); //false
    ensures ko1: (T[1] == \old(T[1])); // false
    ensures ok: (T[2] == \old(T[2])); //true
    */
void call_f_1 (void) {
  int a = 3;
  X = 0;
  f (a);
}

//@ assigns T[m..M];
void array_range (int m, int M);

/*@ ensures ok: (T[1] == \old(T[1]));
    ensures ko: (T[5] == \old(T[5]));
    assigns T[2..5];
*/
void call_array_range (void) {
  int i = 2;
  int j = 5;
  array_range (i, j);
}

//------------------------------------------------------------------------------
/* This is to test that M0 is able to handle a call with a pointer parameter
 * as long as it takes an address as argument. */
/*@ requires \valid(p);
    ensures *p == \old(*p) + 1;
    assigns *p;
*/
void incr (int * p) {
  (*p) ++;
}
//@ ensures \result == x + 1;
int call_incr (int x) {
  incr (&x);
  return x;
}
//@ ensures S.a == \old(S.a) + 1;
void call_incr_on_S (void) {
  incr (&(S.a));
}
//@ ensures T[i] == \old(T[i]) + 1;
void call_incr_on_Ti (int i) {
  incr (T+i);
}
//@ ensures T[0] == \old(T[0]) + 1;
void call_incr_on_T (void) {
  incr (T);
}
//------------------------------------------------------------------------------
// This is to test  [assigns \nothing]
//@ assigns \nothing;
void print (int);

//@ ensures \result == \old(X) + x;
int just_print (int x) {
  print (x);
  return X + x;
}
//------------------------------------------------------------------------------
// This is to test call when assigns are not specified.
// We have to provide a body, else the kernel add a default assigns nothing !
void unknown (void) { return; }

// TODO : this test is wrong at the moment !
/*@
@ ensures ko: X == \old(X); 
   // this one is not provable since we don't know whether [unknown] modifies X
@ ensures ok: \result == X;
*/
int call_unknown (void) {
  unknown ();
  return X;
}
//------------------------------------------------------------------------------

/*@
  requires x >0 ; 
  ensures \result == x;
 */

int f1 (int x)
{
  return x;
}

/*@ ensures \result == 1;
 */
int call_f1 (void)
{ int a = 1 ; return f1(a);}

//------------------------------------------------------------------------------
// This is to check that the result is processed even if it is not specified.

//@ assigns \nothing;
int unknown_result (void);

//@ ensures KO : \result == 0;
int call_unknown_result (void) {
  int x = 0;
  x = unknown_result ();
  return x;
}
//------------------------------------------------------------------------------
// This is to check that the result is correctly processed 
// even if it is specified in 2 post-conditions.

/*@ assigns \nothing;
    ensures \result >= 0;
    ensures \result < 10;
*/
int spec2_result (void);

//@ ensures ok : \result < 20;
int call_spec2_result (void) {
  int x = 0;
  x = spec2_result ();
  return x;
}
//------------------------------------------------------------------------------
