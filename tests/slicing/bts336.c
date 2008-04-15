/* run.config
   OPT: -slice-return main -slice-print -calldeps -journal-disable
   OPT: -main main2 -slice-return main2 -slice-print -calldeps -journal-disable
   OPT: -main main3 -slice-return main3 -slice-print -journal-disable
   OPT: -main main3 -slice-return main3 -slice-print -calldeps -journal-disable
*/
//--------------------------------------
// something to do to have better results...
int T[10];

int f (int i) {
  T[i] ++;
  return T[i];
}

int main (void) {
  int x1 = f(1);
  int x2 = f(2);
  return x2;
}

//--------------------------------------
// Notice that the example below is very similar to the one above :
// f2 also modifies T[1..2], but in this one, the slicing is ok
// because T[1..2] is not in the right part of the assignment.

void f2 (int i) {
  T[i] = i;
}

int main2 (void) {
  f2 (1);
  f2 (2);
  return T[2];
}

//--------------------------------------
// This is a similar problem, but without any array.
// Option -calldeps gives a better result because we can then slice f3 (&c);
// but we cannot slice f3(&a) because it seems to have b as an output,
// and f3 (&b); needs b as an input.
void f3 (int * p) {
  *p += 1;
}

int main3 (void) {
  int a = 1;
  int b = 2;
  int c = 3;
  f3 (&a);
  f3 (&b);
  f3 (&c);
  return b;
}
//--------------------------------------
