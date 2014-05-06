/* run.config
OPT: -scf -check
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
