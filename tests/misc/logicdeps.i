/* run.config
   OPT: -val -memexec-all -inout -inout-callwise -calldeps -no-deps -no-input -no-out
*/

int t[50];

int *p;

//@ assigns t[20..*p+20] \from t[0..*p];
void f();

void g() {
  f();
}

extern int y, z;

void main() {
  //@ assert 0 <= y <= 10;
  //@ assert 15 <= z <= 20;

  p = &y;
  g();
  g();
  g();

  //@ assert \true;
  
  p = &z;
  g();
  g();
  g();
}
