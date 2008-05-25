// this example was contributed by Daniel Zingaro

//@ axiom div2 : \forall int a; 0 < a => 0 <= a/2 < a
//@ axiom mul0 : \forall int a; 0 * a == 0

//@ axiom mul_odd : \forall int a, int b; a%2==1 => a*b == (a/2)*(b*2)+b
//@ axiom mul_even: \forall int a, int b; a%2!=1 => a*b == (a/2)*(b*2)

/*@ requires x >= 0 && y >= 0
  @ ensures
  @   \result == x * y
    @*/
int mult(int x, int y){
  int a = x, b = y, z = 0;
  /*@ invariant 0 <= a && 0 <= b && a * b + z == x * y
    @ variant a */
  while (a > 0) {
    if (a %2 == 1) z += b;
    a /= 2; b *= 2;
  }
  return z;
}
