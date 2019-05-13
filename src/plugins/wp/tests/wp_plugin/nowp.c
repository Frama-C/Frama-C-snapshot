
int main(int a) {
  int b;
  if (a) b = 42 + a;
  //@ assert no_wp: \initialized(&b);
  return b;
}
