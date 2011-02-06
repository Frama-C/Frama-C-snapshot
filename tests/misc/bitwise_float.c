//@ requires 0.05 <= a <= 5.0;
unsigned long main(float a) {
  unsigned long x;
  x = (unsigned long)*((unsigned long *)(& a));
  Frama_C_show_each_x(x);
  x = (*(unsigned long *)(&x)) & 2UL;
  return x;
}
