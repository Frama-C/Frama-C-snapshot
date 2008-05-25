/* run.config
  OPT: -slice-calls send -lib-entry g -slice-print -slicing-level 0 -slicing-debug -no-slice-callers
  OPT: -slice-calls send -lib-entry g -slice-print -slicing-level 1 -slicing-debug -no-slice-callers
  OPT: -slice-calls send -lib-entry g -slice-print -slicing-level 2 -slicing-debug -no-slice-callers
  OPT: -slice-calls send -lib-entry g -slice-print -slicing-level 3 -slicing-debug -no-slice-callers
  OPT: -slice-calls send,send_bis -lib-entry g -slice-print -slicing-level 0 -slicing-debug -no-slice-callers
  OPT: -slice-calls send,send_bis -lib-entry g -slice-print -slicing-level 1 -slicing-debug -no-slice-callers
  OPT: -slice-calls send,send_bis -lib-entry g -slice-print -slicing-level 2 -slicing-debug -no-slice-callers
  OPT: -slice-calls send,send_bis -lib-entry g -slice-print -slicing-level 3 -slicing-debug -no-slice-callers
  OPT: -slice-calls send,send_bis -lib-entry g -slice-print -slicing-level 1
  OPT: -slice-calls send,send_bis -lib-entry g -slice-print -slicing-level 2
  OPT: -slice-calls send,send_bis -lib-entry g -slice-print -slicing-level 3
*/
int G,H,I;

int get (int y) ;

void send(int x);
void send_bis(int x);

void k_bis(int ab, int c, int d) {
  H = c;
  if (ab)
    send_bis (d);
}

int k(int a, int b, int c, int d) {
  int cond = get (d) ;
  G = b;
  k_bis (cond, c, d);
  return a;
}

void g(int b, int c) {
  int r = k(0,0,c,0);
  f(b);
}

int f(int y) {
  k(0,0,0,0);
  int r = k(0,y,0,0);
  int z = k(G,0,0,0);
  //@ slice pragma expr z;
  send (z);
  return z;
}
