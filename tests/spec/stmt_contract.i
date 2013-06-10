int main(int c) {
  int x = 5, y = 2;

  /*@ requires x == 5; */
  /*@ requires y == 2; */
  x = x + y;

  // BTS 1320: \result must be modified to __ret_res, as 
  // assigns also account for abrupt termination
  //@ assigns \result \from x, y;
  if (c) {
    //@ assigns \result \from x;
    return x;
  }
  else {
       // this loop assigns should be rejected though, as loop assigns
       // only speak about successful loop steps.
       //@ loop assigns \result \from y;
       while (1) {
            return y;
       }
  }

  // we should also add an assigns __ret_res here, to match the implicit \result
  //@ assigns x;
  if (c) { x++; return x; }

  // END BTS 1320

  /*@ requires x == 7;  */
  /*@ ensures x == 7; */
  return 0;
}
