/* run.config
OPT: -journal-disable -wp -wp-model Hoare -wp-print -wp-verbose 2
OPT: -journal-disable -wp -wp-model Store -wp-print -wp-verbose 2 -wp-prop assigns -wp-assigns memory
*/
/* run.config_phoare
OPT: -journal-disable -rte -wp -wp-model Hoare -wp-print -wp-verbose 2 -wp-proof alt-ergo
*/

/*----------------------------------------------------------------------------*/

/* This file is to test the strategy generation, so it doesn't need to be tested
 * for different models. Let's choose examples that work with Hoare,
 * except to test assign properties that need Store.
 */

/*----------------------------------------------------------------------------*/
/* we shouldn't be able to prove ko1 from ko2 and then ko2 from ko1 */
/*@ ensures ko1 : \result == x+1;
    ensures ko2 : \result == x+1;
*/
int bts0513 (int x) {
  return x;
}

int bts0513_bis (int x) {
  int i;
  //@ assert ko1 : x > 0;
  //@ assert ok : x > 0;
  return x;
}
/*----------------------------------------------------------------------------*/
// Problem of dependancies : we should be able to prove A, and the proof
// of E shouldn't depend on A !

void dpd1 (int x) {
  //@ ensures Eko: x>0;
  ;
  //@  assert A: x>0;
}

// workaround :
//@ behavior P:
void dpd2 (int x) {
  //@ ensures Eko: x>0;
  ;
  //@ for P: assert A: x>0;
}
//==============================================================================
// specification of an IF block : notice that the proof of the ensures property
// shouldn't depend on [spec_if_f] properties.

int Z;
int T[10];

/*@ assigns T[i]; ensures T[i] > i; */
void spec_if_f (int i);

//@ ensures T[0] > 0;
void spec_if (int c0, int c1, int c2) {
  //@ assigns T[0], Z; ensures T[0] > 0;
  if (c0) { spec_if_f (0); } else { T[0] = 5; }
  //@ assigns T[1], Z;
  if (c1) { spec_if_f (1); } else { Z++; }
  //@ assigns T[2], Z;
  if (c2) { spec_if_f (2); } else { Z++; }
}

//==============================================================================
// when a IF condition is a negation, the AST doesn't have the same structure !
//
void spec_if_cond (int c0) {
  int i;
  //@ ensures i > 0;
  if (c0) { i = 2; } else { i = 1; }
}

void spec_if_not_cond (int c0) {
  int i;
  //@ ensures i > 0;
  if (!c0) { i = 2; } else { i = 1; }
}

//==============================================================================
// Test is unnamed (default) behavior for function and blocks are not mixed
// together.

//@ requires c == 0 ==> x >= 0; ensures \result > 0;
int default_behaviors (int c, int x) {
  int y;

  //@ ensures stmt_p: x > 0;
  if (c) x = 1; 
  else {
    //@ assert x >= 0;
    x++;
  }
  y = 0;
  //@ assert x > y;
  return x;
}
//==============================================================================
