/* run.config
   GCC:
   OPT: -security-slicing -lib-entry -main f -slice-print -journal-disable
   OPT: -security-slicing -security-analysis -lib-entry -main f -journal-disable
   OPT: -security-slicing -security-analysis -security-lattice strong -lib-entry -main f -journal-disable
   OPT: -security-slicing -lib-entry -main f1 -slice-print -journal-disable
   OPT: -security-slicing -security-lattice strong -lib-entry -main f1 -slice-print -journal-disable
   OPT: -security-slicing -lib-entry -main f2 -slice-print -journal-disable
   OPT: -security-slicing -security-lattice strong -lib-entry -main f2 -slice-print -journal-disable
   */

/*@ requires security_status(s) == public; */
void send(int s);

int c, d;

void g(int x, int y) {
  y = (/*@ public */ int) x - x;
  send(y);
  y = 4;
  x = y + 2;
}

int h(int x) {
  send(1);  // faille averee
  return x;
}

void f(void) {
  int x = 0, y, z = 2, t, u, v;
  if (c) { x++; y = x; }
  else { x--; y = (/*@ public */ int) 0; }
  //@ assert (security_status(y) == public); // faille potentielle
  x = 0;
  t = 5;
  y = u = 2 * t;
  z = v = 2 * z; // sliced
  //@ assert (security_status(x) == private); // OK
  if (d) y = (/*@ public */ int) t; else y = (/*@ public */ int) 3 * x;
  send(y); // OK si pas dep. ctrl.
  g(5, 3);
  g(x, y);
  t = h(y);
}

/* ************************************************************************** */

void g1(int x, int y) {
  y = (/*@ public */ int) x - x;
  send(y);
  y = 4;
  x = y + 2;
}

int h1(int x) {
  return x;
}

void f1(void) {
  int x = 0, y, z = 2, t, u, v;
  if (c) { x++; y = x; }
  else { x--; y = (/*@ public */ int) 0; }
  x = 0;
  t = 5;
  y = u = 2 * t;
  z = v = 2 * z;
  if (d) y = (/*@ public */ int) t; else y = (/*@ public */ int) 3 * x;
  g1(5, 3);
  g1(x, y);
  t = h1(y);
}

/* ************************************************************************** */

void g2(int x, int y) {
  y = (/*@ public */ int) x - x;
  y = 4;
  x = y + 2;
}

int h2(int x) {
  return x;
}

void f2(void) {
  int x = 0, y, z = 2, t, u, v;
  if (c) { x++; y = x; }
  else { x--; y = (/*@ public */ int) 0; }
  x = 0;
  t = 5;
  y = u = 2 * t;
  z = v = 2 * z; // sliced
  if (d) y = (/*@ public */ int) t; else y = (/*@ public */ int) 3 * x;
  send(y); // OK si pas dep. ctrl.
  g2(5, 3);
  g2(x, y);
  t = h2(y);
}
