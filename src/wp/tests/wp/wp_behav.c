/* run.config_qualif
OPT: -wp-prop="-qed_ko"
OPT: -wp-prop qed_ko -wp-timeout 2
*/

int X, Y, Z;

/*@
  @ ensures qed_ok: \result > x;
  @ ensures qed_ko: \result > 0;
  @ behavior x1:
  @   assumes x == 1;
  @   ensures qed_ok: \result == 3;
  @   ensures qed_ko: \result == 4;
  @ behavior x2:
  @   assumes x == 2;
  @   ensures qed_ok: \result == 4;
  @   ensures qed_ko: \result == 3;
  @
*/
int f (int x) {
  x++;
  //@ for x1: assert qed_ok: x == 2;
  //@ for x2: assert qed_ok: x == 3;
  return x+1;
}

/*@
    behavior bx:
       assumes x <= y;
       ensures qed_ok: \result == x;
       ensures qed_ko: \result == y;
    behavior by:
       assumes x > y;
       ensures qed_ok: \result == y;
       ensures qed_ko: \result == x;
    complete behaviors bx, by;
    disjoint behaviors bx, by;
*/
int min (int x, int y) {
  return (x < y) ? x : y;
}

/*@ requires n != 0;
  behavior pos:
    assumes n > 0;
    ensures qed_ok: \result == x/n;
  behavior neg:
    assumes n < 0;
    ensures qed_ok: \result == x/-n;
  complete behaviors pos, neg; // notice that this needs the requires hyp
*/
int bhv (int x, int n) {
  n = (n<0) ? -n : n;
  return x/n;
}

/*@ behavior ok: ensures qed_ok: \result > 0;
    behavior ko: ensures qed_ko: \result > 2;
    behavior ko_without_asgn: ensures qed_ko: \result > Y;
*/
int stmt_contract (int c) {
  int x = 0;
  Y = 0;

  /*@ requires qed_ok: x == 0;
    @ ensures qed_ok: x > 0;
    */
  if (c)
    x = 3;
  else
    x = 5;
  return x;
}

//@ ensures qed_ok: \result >= 0;
int stmt_contract_label (int c) {
  int x = 0;

  //@ ensures qed_ok: x >= \old(x);
  if (c) x++;

  return x;
}

/*@ behavior ok: ensures qed_ok: \result > 0;
    behavior ko: ensures qed_ko: \result > 2;
    behavior ok_asgn: ensures qed_ok: \result > Y;
*/
int stmt_contract_assigns (int c) {
  int x = 0;
  Y = 0;

  /*@ requires qed_ok: x == 0;
    @ ensures qed_ok: x > 0;
    @ assigns qed_ok: asgn_ok: x;
    */
  if (c)
    x = 3;
  else
    x = 5;
  return x;
}

int local_named_behavior (int x) {
  int y = 3;
  /*@ behavior xpos:
        assumes x > 0;
        ensures qed_ok: x > 3;
        ensures qed_ok: x > y;
        */
  x += y;
  return x;
}

void assert_needed (int x) {
  //@ assert ko: x > 0;
  int a = 0;
  a += x;
  //@ assert qed_ok: ok_with_hyp: a > 0;
}
  
/* we shouldn't be able to prove ko1 from ko2 and then ko2 from ko1 */
/*@ ensures ko1: \result == x+1;
    ensures ko2: \result == x+1;
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
    behavior b1: assumes 0<n; ensures e1: T[0] == 0;
 */
void razT (int n) {

  //@ loop invariant qed_ok: \forall int k; 0<= k < i ==> T[k] == 0;
  for (int i = 0; i < n; i++) 
    T[i] = 0;
}

//@ ensures qed_ok: ok_with_hoare: T[1] == \old(T[1]);
int more_stmt_assigns (int x) {
  x = 0;
  //@ behavior blk: assigns qed_ok:x, qed_ok:T[x];
  {
    T[x] = 1;
    x = 1;
  }
  return x;
}
/*@ behavior b1:
      assumes x > 0;
      ensures qed_ok: \result > x;
*/
int part_stmt_bhv (int x) {
  /*@ //TODO: not implemented yet.
      for b1: behavior bs: 
                ensures x > \old(x); */
  if (x > 0)
    x++;
  return x;
}

//==============================================================================

