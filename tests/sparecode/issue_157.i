/* run.config
   OPT: -sparecode-debug 1 -sparecode-analysis -journal-disable
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
