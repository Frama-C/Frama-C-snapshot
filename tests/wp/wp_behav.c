
/* run.config_phoare
  OPT:  -journal-disable -rte -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
  OPT:  -journal-disable -rte -wp -wp-model Hoare -wp-proof alt-ergo -wp-fct assert_needed -wp-prop ok_with_hyp -wp-print -wp-verbose 2
 OPT: -journal-disable -rte -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2 -wp-prop ko1
OPT: -journal-disable -rte -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2 -wp-prop e1
COMMENT: next test should be elsewhere in a wp_options.c test file because it is made to test  -wp-prop for une assign property (nothing to do with phoare...)
OPT: -journal-disable -rte -wp -wp-model Store -wp-proof alt-ergo -wp-print -wp-verbose 2 -wp-prop asgn_ok -wp-fct stmt_contract_assigns
*/

int X, Y, Z;

/*@
  @ ensures \result > x;
  @ behavior x1:
  @   assumes x == 1;
  @   ensures \result == 3;
  @ behavior x2:
  @   assumes x == 2;
  @   ensures \result == 4;
  @
*/
int f (int x) {
  x++;
  //@ for x1: assert x == 2;
  //@ for x2: assert x == 3;
  return x+1;
}

/*@
    behavior bx :
       assumes x <= y;
       ensures \result == x;
    behavior by :
       assumes x > y;
       ensures \result == y;
    complete behaviors bx, by;
    disjoint behaviors bx, by;
*/
int min (int x, int y) {
  return (x < y) ? x : y;
}

/*@ requires n != 0;
  behavior pos :
    assumes n > 0;
    ensures \result == x/n;
  behavior neg :
    assumes n < 0;
    ensures \result == x/-n;
  complete behaviors pos, neg; // notice that this needs the requires hyp
*/
int bhv (int x, int n) {
  n = (n<0) ? -n : n;
  return x/n;
}

/*@ behavior ok: ensures \result > 0;
    behavior ko : ensures \result > 2;
    behavior ko_without_asgn : ensures \result > Y;
*/
int stmt_contract (int c) {
  int x = 0;
  Y = 0;

  /*@ requires x == 0;
    @ ensures x > 0;
    */
  if (c)
    x = 3;
  else
    x = 5;
  return x;
}

//@ ensures \result >= 0;
int stmt_contract_label (int c) {
  int x = 0;

  //@ ensures x >= \old(x);
  if (c) x++;

  return x;
}

/*@ behavior ok: ensures \result > 0;
    behavior ko : ensures \result > 2;
    behavior ok_asgn : ensures \result > Y;
*/
int stmt_contract_assigns (int c) {
  int x = 0;
  Y = 0;

  /*@ requires x == 0;
    @ ensures x > 0;
    @ assigns asgn_ok: x;
    */
  if (c)
    x = 3;
  else
    x = 5;
  return x;
}

int local_named_behavior (int x) {
  int y = 3;
  /*@ behavior xpos :
        assumes x > 0;
        ensures x > 3;
	*/
  x += y;
  return x;
}

void assert_needed (int x) {
  //@ assert ko : x > 0;
  int a = 0;
  a += x;
  //@ assert ok_with_hyp : a > 0;
}
  
/* we shouldn't be able to prove ko1 from ko2 and then ko2 from ko1 */
/*@ ensures ko1 : \result == x+1;
    ensures ko2 : \result == x+1;
*/
int bts0513 (int x) {
  return x;
}

//@ assigns X, Y;
void unknown (int, int);

//@ ensures \result > X;
int stmt_assigns (int a) {
  int x = 0;
  int y = 3;
  X = x;
  //@ assigns Y;
  unknown (x, y);
  x = x+1;
  return x;
}

int T[10];

// use Inv as Hyp for Bhp props
/*@ requires n < 10;
    behavior b1 : assumes 0<n; ensures e1: T[0] == 0;
 */
void razT (int n) {

  //@ loop invariant \forall int k; 0<= k < i ==> T[k] == 0;
  for (int i = 0; i < n; i++) 
    T[i] = 0;
}

//@ ensures ok_with_hoare: T[1] == \old(T[1]);
int more_stmt_assigns (int x) {
  x = 0;
  //@ behavior blk: assigns x, T[x];
  {
    T[x] = 1;
    x = 1;
  }
  return x;
}
//==============================================================================
