/* run.config
OPT: -wp-model Hoare -wp-no-simpl -wp-prop Rmain
OPT: -wp-model Hoare -wp-no-simpl -wp-fct main 
OPT: -wp-model Hoare -wp-no-simpl -wp-prop Rf
OPT: -wp-model Hoare -wp-no-simpl -wp-fct double_call
OPT: -wp-model Hoare -wp-no-simpl -wp-fct stmt_pre -wp-prop Rstmt 
*/

/* run.config_qualif
OPT: -journal-disable -wp -wp-model Store -wp-par 1
*/

int G = 3;

//@ requires qed_ok: Rf: a > 0; ensures Ef: \result > 0;
int f (int a);

// Function with 2 preconditions.
//@ requires qed_ok: Rga: a > 0; requires Rgb: b > 0; ensures Ef: \result > 0;
int g (int a, int b);

/* This is to test explicitly the case where the post-condition of a call
 * is at the same program point than the precondition of the next call */
//@ requires Rd: x >= 0;
int double_call (int x) {
  int x1 = f (x+1);
  int x2 = f (x+2);
  return x1 + x2;
}

//@ requires qed_ok: Rmain: G > 0; ensures qed_ok: Emain: \result > 0;
int main (void) {
  int x = f(G);
  return x;
}

//@ ensures qed_ok: \result > 0;
int call_main (void) {
  G = 1;
  return main ();
}

//@ requires 0 < G;
int stmt_pre (void) {
  int x = 0;
  //@ requires qed_ok: Rstmt: G > x;
  x = G - x;
  return x;
}

// proving the preconditions on [g] call from the GUI should change the
// status of [g] preconditions since it is the only call.
int call_g (void) {
  int x = 1;
  int y = 2;
  return g (1, 2);
}
