/* run.config
   STDOPT: +"-impact-pragma main -pdg -pdg-print"
   */

//@ requires \false;
void f() { // Bottom PDG
}

void main(int c) {
  /*@ impact pragma stmt; */
  int x = 1;
  int y, z;
  if (c) {
    y = x;
    f();
    z = x;
  }
  z = x;
}
