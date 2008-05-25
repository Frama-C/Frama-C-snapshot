/* run.config
   GCC:
   OPT: -security-analysis -lib-entry f -security-propagate-assertions
   OPT: -security-analysis -lib-entry g -security-propagate-assertions
   OPT: -security-analysis -lib-entry h -security-propagate-assertions
   */

/*@ requires security_status(x) == public(); */
void send(int x);

void f(int y) {
  send(y);
}

/*@ requires security_status(z) == public(); */
void g(int z) {
  f(z);
}

/*@ requires security_status(t) != public(); */
void h(int t) {
  g(t); /* security leak */
}
