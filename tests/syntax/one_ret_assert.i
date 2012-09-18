int X;

void f(void) {
  X++;
}

int g(void) {
  X++;
}

int h(void) {
  if (X) { return 3; } else { return 4; }
}

int main() {
  X = h();
  f();
  return g();
}
