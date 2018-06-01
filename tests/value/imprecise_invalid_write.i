int x;

int main1(int c)
{
  *(int*)c = x;
}

int main2() {
  void *p = &main1 + (int)&main1;
  *((int *)p) = 1;
}

int *s = "abc";

int main3() {
  int *p = s+(int)s;
  *p = 1;
}

void main(int c) {
  if (c & 17) {
    main1(c);
  }
  if (c & 19) {
    main2();
  }
  if (c & 21) {
    main3();
  }
}
