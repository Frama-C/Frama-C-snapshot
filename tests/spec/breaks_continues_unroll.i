/* run.config
*/
// Semantics of unrolling
int unroll (int c) {
  int x = 0;
  //@ loop pragma UNROLL 1;
  while (1) {
    /*@ breaks \false;
        continues x == \old(x) + 1; */
    switch (x) {
      /*@ breaks x == 13; */
      { 
      case 11: x++; continue;
      case 12: x++;
      case 13: break;
      default:;
      }}
    /*@ breaks x == \old(x);
        continues x == \old(x) + 1;
    */
    {
      if (x < c) { x++; continue; }
      break;
    }
  }
  return x;
}	
