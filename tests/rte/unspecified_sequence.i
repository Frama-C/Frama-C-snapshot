unsigned long long f(int x) {
  return 0;
}

int t[10];

//@ requires x < 1 << 30;
void main(int x) {
  unsigned long long v;
  int y = t[(int)f(x+1)];
  int z = t[(int)f(x+1)+(int)f(x)];
}
