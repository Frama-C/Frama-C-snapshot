/* run.config
   OPT: -wp -wp-model Hoare -wp-print

   run.config_phoare
   OPT:  -journal-disable -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
*/

//------------------------------------------------------------------------------
// This file is to test the preconditions verification                        
//------------------------------------------------------------------------------

int X; // even if not initialized, we should be able to check that it is 0.
int X2 = 2;
int T[10];

//@ requires X >= 0; ensures X > 0;
void f (void) {
  X++;
}

/*@ requires X2 == 2;
  @ requires X == 0;
  @ requires \valid (&X);
  @ requires \valid (&(T[0..9]));
*/
int main (void) {
  f ();
  return 0;
}

//------------------------------------------------------------------------------
