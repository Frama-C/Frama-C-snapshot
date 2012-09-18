/* run.config
  OPT: -val -inout-callwise -inout -pdg -pdg-print -calldeps -deps -then -slice-return main -then-on "Slicing export" -print
*/

int Y, X;
volatile v;

//@ assigns \result \from \nothing;
int input();

void f (void) {
  int l = 0;
  Y = input ();
  if (l > 0) {
    Y ++;
  }
  //@ assert Y > 0;
}

//@ ensures \false;
void g() {
  while(1);
}

void h() {
  if (v) g();
  else  X = X + 2; // X is a sure output, as the other branch does not return
}

int main (void) {
  Y = 3; // Dead when slicing on the value of Y at the end of main
  f ();
  h();
  return Y;
}
