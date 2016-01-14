/* run.config
   STDOPT: +"-metrics-value-cover" +"-metrics-cover main"
   STDOPT: +"-metrics-value-cover" +"-main foobar" +"-metrics-cover foobar"
**/

void (*bar) (int);  extern void (*bar_extern) (int);

void baz (int j) { return; }

int foobar () {
  bar = baz;
  bar (2);
  return 0;
}

void foo (int k) {
  int i = 0;
  return;
}

/* foo is unreachable since j is always 0 */
int main() {
  int j = 0;
  if (!j) {
    return 1;
  }
  else {
    if (bar == bar_extern) exit (1);
    bar = foo;
    bar (1);
    return 0;
  }
}
