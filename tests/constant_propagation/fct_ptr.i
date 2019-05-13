/* run.config
   STDOPT: +"-then-on propagated"
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
