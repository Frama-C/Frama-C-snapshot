/* run.config
   GCC:
   OPT: -security-analysis -lib-entry -main f -security-propagate-assertions -journal-disable
   OPT: -security-analysis -lib-entry -main g -security-propagate-assertions -journal-disable
   OPT: -security-analysis -lib-entry -main h -security-propagate-assertions -journal-disable
   */

/*@ requires security_status(x) == public; */
void send(int x);

void f(int y) {
  send(y);
}

/*@ requires security_status(z) == public; */
void g(int z) {
  f(z);
}

/*@ requires security_status(t) != public; */
void h(int t) {
  g(t); /* security leak */
}
