int f(int);

void *p = f;


int main () {
  int (*q)(int) = (void *)0xfff45;
  q(2);
  q = p;
  q(3);
}
