int f(int);

void *p = f;

int (*pf) (int x) = f;

int g() {
  return ((int (*)(int))(*pf))(4);
}

int main () {
  int (*q)(int) = (void *)0xfff45;
  q(2);
  q = p;
  q(3);
}
