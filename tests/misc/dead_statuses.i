/* run.config
   STDOPT: +"-load-module lib/plugins/Report -then -report"
*/

//@ requires \true; assigns \nothing;
void f(void);

void main(int c) {
  f();
  if (c >= 0) {
    f();
    if (c < 0) {
      f ();
      int t[3] = {1}; // Test statuses of behaviors, that are logical consequences
      //@ assert \false;
      //@ loop invariant \false;
      while (1) {
        //@ requires \false;
        c = 0;
      }
    }
  }
}
