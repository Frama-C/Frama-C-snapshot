/*  run.config
OPT: -check -slice-assert main -then-on "Slicing export" -print 
*/

int X = 1 ;

int main(void) {
  int y;
L: y = 0;
   X++;
  //@ assert X > \at(X,L);
  return X;
}
