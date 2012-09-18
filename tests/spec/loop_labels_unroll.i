int main () {

  int x = 0;

  /*@ 
    loop pragma UNROLL 4;
    loop invariant \at(x,LoopEntry) == 0;
    loop invariant \at(x,LoopCurrent) <= 15;
  */
  while (x<15) {
    x++;
    /*@ assert x == \at(x,LoopCurrent) + 1; */
    int i = 0;
    /*@ loop invariant \at(i,LoopEntry) == 0; */
    while (i<4) { i++; /*@ assert \at(i,LoopCurrent) == i-1; */ }
    /*@ assert i > 0; */
  }
}
