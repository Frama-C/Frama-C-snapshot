/* run.config*
   STDOPT: +"-eva-memexec -calldeps -no-deps -no-input -no-out -then -inout"
*/

int t[50];

int *p;

//@ assigns t[20..*p+20] \from t[0..*p];
void f(void);

void g() {
  f();
}

extern int y, z;

void main() {
  //@ assert 0 <= y <= 10;
  //@ assert 15 <= z <= 20;

  p = &y;
  g(); // t[20..(20-30)] \from t[0..10]
  g();
  g();

  //@ assert \true;
  
  p = &z; // t[20..(35-40)] \from t[0..20]
  g();
  g();
  g();
}
