/*  run.config
OPT: -check -slice-return main -then-on "Slicing export" -print 
*/
void f(void) { return; }
int X = 1 ;
int main(void) {
 call: f();
  //@ assert X == \at(X,call);
  return X; 
}
