/* run.config
   GCC:
   OPT: -security-analysis -security-lattice strong -lib-entry h
   */

/*@ requires security_status(s) == public(); */
void send(const int s);

int a,b;
void f(void) {
  a = (int /*@ public */) 0;
  if (a) b = 0; else b = 1;
  send(a);                   // faille potentielle si dep. de ctrl. :
                             // déduction d'info sur b
}

int c,d;
void g(void) {
  c = (int /*@ public */) 0;
  send(c);                   // faille potentielle si dep. de ctrl.
                             // déduction d'info sur d
  if (c) d = 0; else d = 1;
}

int h(void) {
  f();
  g();
  return 0;
}
