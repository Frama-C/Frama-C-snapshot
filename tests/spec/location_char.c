/* run.config
EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
OPT: -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/

/*@ requires x <= 0;
    ensures  \result == x;
*/
int f(int x) { //@ assert x<=0;
  int y = x;
  /*@ assert y <= 0 &&
             x == y; */
  return y;
}

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
                         /*@ assert outer.inner.a == 0; */
                  /*@ assert outer.b ==
        outer.inner.a &&
                 0 != 1;
   */
  return 0;
}
