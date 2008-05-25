//@ axiom distr_right: \forall int x, int y, int z; x*(y+z) == (x*y)+(x*z);
//@ axiom distr_left: \forall int x, int y, int z; (x+y)*z == (x*z)+(y*z);

/*@ requires x >= 0 && y > 0;
  @ ensures  \exists int r; x == \result * y + r && 0 <= r < y;
  @*/
int division(int x, int y) {
  int i = 0, j = x;
  /*@ loop invariant x == i * y + j && 0 <= j;
    @ loop variant   j;
    @*/
  while (j >= y) {
    i++;
    j -= y;
  }
  return i;
}


/* 
Local Variables:
compile-command: "LC_ALL=C make division"
End:
*/
