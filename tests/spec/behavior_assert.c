/* run.config 
OPT: -memory-footprint 1 -val -deps -out -input -journal-disable -lib-entry
OPT: -memory-footprint 1 -val -deps -out -input -journal-disable    
*/

int e;

/*@
  behavior b:
  assumes e==0;
*/
void f(void) {
  int x = 1;
  //@ for b: assert \false;
  x = 2;
  //@ for b: assert 1==1;
  x = 3;
}

int G;
/*@
  behavior be:
  assumes e==0;
  ensures G==3;
*/
void g(void) {
  G = 3;
}

void main(void) {
  f();
  g();
}
