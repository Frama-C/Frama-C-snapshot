/* run.config
   GCC:
   OPT: -security-slicing -lib-entry -main backward_from_called -slice-print -journal-disable
   OPT: -security-slicing -lib-entry -main backward_caller -slice-print -journal-disable
   OPT: -security-slicing -security-lattice strong -lib-entry -main forward_from_called -slice-print -journal-disable
   OPT: -security-slicing -security-lattice strong -lib-entry -main forward_caller -slice-print -journal-disable
*/

/*@ requires security_status(s) == public; */
void send(int s);

int A, B, C, D;

/* ************************************************************************* */

void backward_called(int *x) {
  send(A);
  send(*x);
}

void backward_from_called() {
  int a = 2;
  A = 1;
  B = A;
  C = a;
  backward_called(&a);
}

/* ************************************************************************* */

void backward_from_caller(int *x) {
  A = 1;
  B = A;
  D = *x;
  *x = 3;
  C = *x;
}

void backward_caller() {
  int a = 2;
  A = 0;
  backward_from_caller(&a);
  send(A);
  send(a);
}

/* ************************************************************************* */

void forward_called(int *x) {
  int y = 2;
  send(A);
  send(y);
  C = A;
  *x = A;
  D = y;
}

void forward_from_called() {
  int a = 2; // sliced
  D = 0;     // sliced
  A = 1;
  forward_called(&a);
  B = C;
  C = a;
  D++;
}

/* ************************************************************************* */

void forward_from_caller(int *x) {
  int *y = x;
  D = B + A;
  B = *y + 1;
  *y = 2;     // should be sliced
  C = A;
  D = *x;     // should be sliced
}

void forward_caller() {
  int a = 2;
  B = 1;
  A = 0;
  send(A);
  send(a);
  forward_from_caller(&a);
}
