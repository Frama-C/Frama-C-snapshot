/* run.config
OPT: -copy -print
*/
struct inner {
  int a, b;
  char c;
  void *p;
};

struct outer {
  struct inner *pinner;
  struct inner inner;
  int a;
  struct inner ainner[5];
  int b;
  char c;
  long long l;
};

int main (void)
{
  struct inner inner;
  struct outer outer;
  outer.inner.a = 0;
  return 0;
}
