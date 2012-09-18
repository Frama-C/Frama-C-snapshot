int G = 0;

const char* foo = "foo";

void test(const char */*name*/);

void test2(int x) {
  /*@ ghost
    int y = 0;
    if (x>0) { y = x * x; };
  */
  G = x * x;
  test(foo);
}
