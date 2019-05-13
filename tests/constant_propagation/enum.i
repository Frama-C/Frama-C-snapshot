/* run.config
   STDOPT:
*/

enum E { A, B, C, D };

int f(enum E e) { return e + 1; }

int main (int c, unsigned u) {
  enum E x = A;
  int y = f(x);
  int z = f(D);
  int t = B + c;
  int v = C + u;
  return y+z+t+v;
}
