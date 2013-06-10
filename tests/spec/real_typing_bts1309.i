void foo(int c) {
  float f = 1.0;
  /*@ assert 0.0 <= (c ? f : 2.0); */
}
