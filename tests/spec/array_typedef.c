/*run.config
 OPT: -print -val -journal-disable
 */
#define IP_FIELD 4
typedef int ip_address[IP_FIELD];

typedef struct {
  ip_address src;
  int dst[IP_FIELD];
} msg;

/*@ assigns \empty; */
void send_addr(const ip_address addr);
void send_msg(const msg *msg) {
  send_addr(msg->src);
}

void host_address(ip_address ip) {
  unsigned int i = sizeof (int [4]) / sizeof (int);
  ip[0] = 192; ip[1] = 100; ip[2] = 200; ip[i - 1] = 101;
  // @ assert ip[(sizeof (int [4]) / sizeof (int)) - 1] == 101;
}

void create_msg(msg *msg) {
  host_address(msg->src);
  host_address(msg->dst);
  //@ assert msg->dst[0] == 192;
  //@ assert msg->src[0] == 192;
  //@ assert msg->dst[(sizeof (ip_address) / sizeof (int)) - 1] == 101;
  // @ assert msg->src[(sizeof (int [4]) / sizeof (int)) - 1] == 101;
}

int main() {
  msg msg1;
  create_msg(&msg1);
  send_msg(&msg1);
  return 0;
}
