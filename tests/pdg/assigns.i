/* run.config
  OPT: -journal-disable -val-show-progress -pdg -pdg-print -calldeps -pdg-verbose 2
*/

unsigned int x, y;

//@ assigns x \from x;
void f(void);

int main() {
  x = 1U;
  y = 2U;
  f();
  y = y + 2;
  return x+y; // There must be a data dependency edge to x = 1, but not y=2;
}
