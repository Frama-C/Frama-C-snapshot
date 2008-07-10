/* run.config
   GCC:
   OPT: -security-slicing -lib-entry -main f -slice-print
   OPT: -security-slicing -lib-entry -main g -slice-print
   OPT: -security-slicing -lib-entry -main h -slice-print
   OPT: -security-slicing -lib-entry -main toto -slice-print
   OPT: -security-slicing -security-lattice strong -lib-entry -main toto -slice-print
   */

/* ************************************************************************* */

/*@ requires security_status( *x ) == public; */
void send(int *x) ;

void f() {
  int x = 0;
  send(&x);
}

/* ************************************************************************* */

/*@ ghost int channel; */

/*@ requires security_status( *x ) == public;
  assigns channel \from *x ; */
void send2(const int *x);

void g() {
  int x = 0;
  send2(&x);
}

/* ************************************************************************* */

void h_tmp(int *x) {
  *x = 1;
}

void send_tmp(int *x) {
  send(x);
}

void h() {
  int x = 0;
  h_tmp(&x);
  send_tmp(&x);
}

/* ************************************************************************* */

typedef struct {
  int src[2];
} msg;

/*@ requires security_status(addr) == public;
  assigns channel \from addr[0]; */
void send_addr(const int addr[1]);

void send_msg(msg *msg) {
  send_addr(msg->src);
}

void create_msg(msg *msg) {
  msg->src[0] = 19;
}

void toto() {
  msg msg1;
  int x;
  msg1.src[0] = 10;
  create_msg(&msg1);
  send_msg(&msg1);
  x = msg1.src[1];
}

/* ************************************************************************* */
