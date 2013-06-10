/* run.config
   STDOPT: +"-metrics-by-function" +"-metrics-value-cover"
**/

void (*bar) (int); void (*t[2])(int);

void baz (int j) { return; }

void (*t[2])(int)= {
  baz,
  0};

void foo (int k) {
  int i = 0;
  return;
}

/* foo is unreachable since j is always 0; baz is not called */
int main() {
  int j = 0;
  void (*(*pt)[2])(int) = &t;
  if (!j) {
    return 1;
  }
  else {
    bar = foo;
    bar (1);
    return 0;
  }
}
