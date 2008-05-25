/* run.config
   GCC:
   OPT: -security-slicing -lib-entry f -slice-print
   OPT: -security-slicing -security-lattice strong -lib-entry f -slice-print
*/

/*@ requires security_status(s) == public(); */
void send(int s);

int a, b;

void g(int x, int y) {
  a = x;
  b = 2 * y;
}

void f(void) {
  int x = 5;
  int y = 6;
  g(x, y);
  send(y);
}
