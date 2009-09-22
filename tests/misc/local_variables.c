int main (int c, int * p) {
  if (c) {
    int x = 1;
    p = &x;
  }
  {
    int y = 0;
    { int z = 1;
      int t = y + z;
    }
  }
  for (int i = 0; i<5; i++) {
    int a = 0;
    a += i;
  }
  return *p;
}
