//@ ensures \result == p + (\let idx = i ; idx + 0);
int * f(int * p, int i) {
  return p + i;
}
