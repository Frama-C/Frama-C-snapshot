/* run.config
   GCC:
   OPT: -security-slicing -lib-entry -main f -slice-print
   */

/*@ requires security_status(s) == public; */
void send(const int s, const char *msg);

int x;

void f() {
  char *msg = "msg";
  send(x, msg);
}
