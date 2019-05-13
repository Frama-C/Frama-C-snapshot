/* run.config
   STDOPT: +"-sparecode-analysis"
*/

int f() {
  return 0;
}

int X;
void g() {
  f();
}

int main(void) {
  int x;
  g();
  x = f();
  return x;
}
