/* run.config
   GCC:
   OPT: -security-analysis -lib-entry -main f -security-lattice weak -journal-disable
   OPT: -security-analysis -lib-entry -main f -security-lattice strong -journal-disable
   OPT: -security-analysis -lib-entry -main f -security-lattice strong -security-propagate-assertions -journal-disable
   */

/*@ requires security_status(x) == public; */
void send(int x);

int c, x, y;

void g() { y = 1; send(y); /* faille toujours d�tect�e : 1 priv� */ }
void h() { y = 2; g(); }

void g2(int a) { a = (int /*@ private */) 1; send(a); /* faille detectee */ }
void g3(int a) { int b = a; send(b); }

void f() {
  y = (int /*@ public */) 0;
  g();
  x = (c ? (int /*@ public */) 0 : (int /*@ public */) 1);
  send(x);    /* faille potentielle sur x si dep de ctrl */
  if (c) { g(); } else h();
  send(x);    /* faille potentielle sur x si dep de ctrl */
  send((int /*@ public */) y);    /* faille sur y si dep de ctrl */

  g2((int /*@ public */) 10);
  g3((int /*@ public */) 11);
  g3((int /*@ private */) 12);
}
