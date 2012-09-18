/* run.config
   OPT: -check -slice-return main -calldeps -journal-disable -then-on 'Slicing export' -print
   OPT: -check -main main2 -slice-return main2 -calldeps -journal-disable -then-on 'Slicing export' -print
   OPT: -check -main main3 -slice-return main3 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -journal-disable -main main3 -inout -inout-callwise -calldeps -slice-return main3  -then-on 'Slicing export' -print
   OPT: -check -journal-disable -main main -calldeps -inout-callwise -slice-return main -then-on 'Slicing export' -print
   OPT: -check -journal-disable -main main4 -calldeps -inout-callwise -slice-return main4 -then-on 'Slicing export' -print
   OPT: -check -journal-disable -main main4 -calldeps -inout-callwise -slice-return main4 -slicing-level 3 -then-on 'Slicing export' -print
   OPT: -check -journal-disable -main main5 -calldeps -inout-callwise -slice-return main5 -then-on 'Slicing export' -print -calldeps -inout-callwise -slice-return main5 -then-on 'Slicing export 2' -print
*/
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

// more complicated variant of 'main3'. This has been resolved in the same way as the first 'main' of this file
void f4 (int * p, int* q) {
  *p += 1;
  *q += 1;
}

int main4 (volatile int c) {
  int a1 = 1;
  int b1 = 2;
  int a2 = 3;
  int b2 = 4;
  int a3 = 5;
  int b3 = 6;
  int a4 = 7;
  int b4 = 8;
  int a5 = 9;
  int b5 = 10;
  while(c) {
    f4 (&a1, &b1);
    f4 (&a2, &b2);
    f4 (&a3, &b3);
    f4 (&a4, &b4);
    f4 (&a5, &b5);
  }
  return a2+b4;
}
//--------------------------------------
// Non-optimal example if only one phase of slicing is done. Would need a notion of "operational functional dependencies", or a callwise pdg

int x5;

void f5(int *p, int *q) {
  (*p)++;
  x5 = *q;
}

int main5() {
  int a1=1, a2=2, b1=3, b2=4;
  f5(&a1, &b1); // This call should be sliced away
  f5(&a2, &a1);

  return a2;
}
