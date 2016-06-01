int G,H;

int h(int *argh) {
//  G = *argh;
  *argh = H;
  return H;
}

int g() {
  int ga;
  h(&ga);
  return 0;
}

int f() {
  int fa;
  h(&fa);
  return 0;
}

int main() {
  f();
  g();
}
