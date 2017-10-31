/* run.config
   STDOPT: +"-calldeps -then -impact-pragma main"
   */

int a, r1, r2;

void f() {
  a = 1;
}

void aux(int *p, int cond) {
  if(cond)
    r1 = *p;
  else
    r2 = *p; // Ne devrait pas être sélectionné
}

void g1() {
  aux(&a, 0);
}

void g2() {
  aux(&a, 1);
}

void main () {
  g1();
  //@ impact pragma stmt;
  f();
  g2();
}
