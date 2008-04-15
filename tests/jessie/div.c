
/*@ lemma mult_div: \forall integer i, j; i != 0 ==> (i * j) / i == j;
  @ */

/*@ requires x > 0;
  @ ensures 4 * x / 4 >= 0;
  @*/
int f(int x) {
  return 10/x;
}
