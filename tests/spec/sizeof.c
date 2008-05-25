/*@ requires i > sizeof(int);
  @ ensures \result > sizeof(i);
  @*/
int f(int i) {
  return i;
}
