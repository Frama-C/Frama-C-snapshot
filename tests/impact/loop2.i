/* run.config
   STDOPT: #"-impact-pragma main -lib-entry -calldeps" +"-then -ulevel 10"
   */


volatile v;

int t[10], u[10], w[10];

void init() {
  for (int i=0; i<10; i++) {
    u[i] = v;
  }
}

void f(int i) {
  int v = t[i]; // should not be selected
  w[i] = i; // should not be selected (selection depends on if (t[i]) in main
  t[i] = u[i];
}

void main() {
  //@ impact pragma stmt;
  init ();
  for (int i=0; i<10; i++) {
    if (t[i]) // should not be selected
      f(i);
  }
}
