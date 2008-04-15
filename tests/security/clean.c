/* run.config
   GCC:
   OPT: -security-analysis -lib-entry -main f -security-lattice weak -journal-disable
   OPT: -security-analysis -lib-entry -main f -security-lattice strong -journal-disable
   */

int c;

/*@ requires security_status(x) == public; */
void send(int x);

int y;
int x;

int g() { int a = y; return a; }
int h(int b) { int c = g(); return b + c; }

void f() {
  int t;
  if (c) x = (int /*@ public */) 0; else x = (int /*@ public */) 1;
  send(x);      /* faille potentielle sur x si dep de ctrl */
  y = (int /*@ public */) 0;
  send(g());
  send(h(g()));
  send(h(g()));
  if (c) y = (int /*@ public */) 1;
  send(y);      /* faille potentielle sur y si dep de ctrl */
  send(h(g())); /* faille potentielle sur y si dep de ctrl */
}
