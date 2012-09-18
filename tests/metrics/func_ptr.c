/* run.config
   STDOPT: +"-metrics-value-cover" +"-metrics-cover main"
   STDOPT: +"-metrics-value-cover" +"-main foobar" +"-metrics-cover foobar"
**/

void (*bar) (int);

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
    bar = foo;
    bar (1);
    return 0;
  }
}
