int main(void) {
  int x = 5, y = 2;

  /*@ requires x == 5; */
  /*@ requires y == 2; */
  x = x + y;

  /*@ requires x == 7;  */
  /*@ ensures x == 7; */
  return 0;
}
