/*@ requires y == 12 ; */
int main(int y) {
  int x;
  x = y;
  //@ assert x == 12;
  if (x == 0) x++; else x = 1;
  //@ assert x == 1;
  return 0;
}
