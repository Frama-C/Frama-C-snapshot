int g(int);

void f(void) {
  int x = 0;
  switch (1) {
  case 1: x = (int)g(x);
  }
}
