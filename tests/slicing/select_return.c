/* run.config
  OPT: -slice-calls send -lib-entry -main g -slice-print -slicing-level 0 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-calls send -lib-entry -main g -slice-print -slicing-level 1 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-calls send -lib-entry -main g -slice-print -slicing-level 2 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-calls send -lib-entry -main g -slice-print -slicing-level 3 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-calls send,send_bis -lib-entry -main g -slice-print -slicing-level 0 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-calls send,send_bis -lib-entry -main g -slice-print -slicing-level 1 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-calls send,send_bis -lib-entry -main g -slice-print -slicing-level 2 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-calls send,send_bis -lib-entry -main g -slice-print -slicing-level 3 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-calls "send , send_bis" -lib-entry -main g -slice-print -slicing-level 1 -journal-disable
  OPT: -slice-calls "send, send_bis" -lib-entry -main g -slice-print -slicing-level 2 -journal-disable
  OPT: -slice-calls "send ,send_bis" -lib-entry -main g -slice-print -slicing-level 3 -journal-disable
  OPT: -slice-return f   -lib-entry -main g -slice-print -slicing-level 0 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-return f   -lib-entry -main g -slice-print -slicing-level 1 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-return f   -lib-entry -main g -slice-print -slicing-level 2 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-return f   -lib-entry -main g -slice-print -slicing-level 3 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-pragma f   -lib-entry -main g -slice-print -slicing-level 0 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-pragma f   -lib-entry -main g -slice-print -slicing-level 1 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-pragma f   -lib-entry -main g -slice-print -slicing-level 2 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-pragma f   -lib-entry -main g -slice-print -slicing-level 3 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-value  H   -lib-entry -main g -slice-print -slicing-level 1 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-value  H   -lib-entry -main g -slice-print -slicing-level 2 -slicing-debug -no-slice-callers -journal-disable
  OPT: -slice-value  H   -lib-entry -main g -slice-print -slicing-level 3 -slicing-debug -no-slice-callers -journal-disable
*/

int G,H,I;

int get (int y) ;

void send(int x);

void send_bis(int x);

int k(int a, int b, int c, int d) {
  int cond = get (d) ;
  G = b;
  H = c;
  if (cond)
    send_bis (d);
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
