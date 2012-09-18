int main () {

  int x = 0;

  /*@ loop invariant \at(x,LoopEntry) == 0;
      loop invariant \at(x,LoopCurrent) <= 15;
  */
  while (x<15) {
    x++;
    /*@ assert x == \at(x,LoopCurrent) + 1; */
  }

  // Rejected.
  /*@ assert \at(x,LoopEntry) == 0; */

}
