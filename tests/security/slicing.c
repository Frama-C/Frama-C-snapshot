/* run.config
   GCC:
   OPT: -security-slicing -lib-entry -main f -slice-print -journal-disable
   OPT: -security-slicing -security-analysis -security-lattice strong -lib-entry -main f -journal-disable
*/

/*@ requires security_status(s) == public; */
void send(const int s);

int c, x, y, z = 1;

void g(void) { y = 1; send(y); /* faille toujours détectée : 1 privé */ }

void f(void) {
  int a, b = 2;
  y = (int /*@ public */) 0;
  a = b + 1;
  g();
  x = (int /*@ public */) (c ? 0 : 1);
  send(x);    /* faille potentielle sur x si dep de ctrl */
  b += z;
  if (c) { g(); }
  send(x);    /* faille potentielle sur x si dep de ctrl */
  send((int /*@ public */) y);    /* faille sur y si dep de ctrl */
  y = 5;
  c = y;
}
