/* run.config
OPT: -scf -check -then-on propagated -check
*/

void (*pf)(void);

void g() {
  pf ();
}

void f(void);

int main() {
  pf = f;
  g();
}
