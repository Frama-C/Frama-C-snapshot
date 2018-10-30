/* run.config
STDOPT: +"-kernel-msg-key printer:unspecified"
*/

int f(int *p, int x) {
  *p = x + 1;
  return x;
}

int g() {
  int x = 3;
  int y = f(&x, ++x); // correct: we're not reading x, but &x
  int a[10] = { 0 };
  int *b = a;
  int z = f(&b[x], b[2]);
  int t = f(&b[x], ++x); // incorrect: write and read access to x;
  int u = f(&b[2], *(b++)); // incorrect: write and read access to b;
  return y;
}
