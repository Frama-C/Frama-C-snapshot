/* run.config
   GCC:
   OPT: -security-slicing -lib-entry check_and_send -slice-print
   OPT: -security-slicing -lib-entry check_and_send -security-lattice strong -slice-print
*/

/*@ requires security_status(data) == public(); */
void send(const void *data);

void check_and_send(char msg[]) {
  int a;
  send(msg);
  a = msg[0];
  if (msg[0] == 's')
    a = 1;
}
