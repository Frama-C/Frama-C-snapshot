/* run.config
   GCC:
   OPT: -security-slicing -slice-print
   */

typedef struct {
  char *data;
} msg;

msg msg3;

/*@ requires security_status(data) == public();
  assigns \nothing; */
void send_data(char *data);

void send_msg(msg msg) {
  msg3.data = "titi";
  send_data(msg.data);
}

void main() {
  msg msg1,msg2;
  msg2.data = "toto";
  send_msg(msg1);
}
