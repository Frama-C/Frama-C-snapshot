int x;

/*@ axiomatic A {
  predicate E(integer v) = x == v;
  predicate P{L1,L2}(integer i) = \at(x,L1) == \at(x,L2)+ i;
  predicate Q{L1,L2}(integer i) = \at(x == \at(x,L2)+ i,L1);
  axiom idem{L1,L2}: \forall integer i ; P{L1,L2}(i) <==> Q{L1,L2}(i);
  } */

/*@ ensures x == 2+\old(x)+y;
    ensures \at(E(\at(x-2-y,Here)),Pre);
*/
int f(int y) {
  x += y;
 L1:
  x++;
  //@ ghost L2: ;
  x++;
  //@ assert \at(x,L1) == \at(x,Pre)+y;
  //@ assert \at(x,L2) == 1+\at(x,Pre)+y;
  //@ assert P{Here,Pre}(2+y);
  return x;
}

void test () {
  int x = 0;
  L1: {
  int x = 1;
  L2:
  // assert below speaks about two distinct x.
  /*@ assert \at(&x, L1) != \at(&x,L2); */
  x = 2;
  }
}

void ko (int z) {
  L: {
    int y = 0;
    // assert below should not typecheck: y is not in scope at L (nor at Pre)
    //@ assert KO: \at(y,L) == 0;
    //@ assert KO: \at(y,Pre) == 0;
    //@ assert KO: \at(z,Init) == 0; // at Init, only globals are in scope
    //@ assert OK: \at (x,Init) == 0;
    //@ assert OK: \at(z,Pre) == 0;
  }
  while (x>0) {
    int i = 1;
    x--;
    //@ assert KO: \at(i,LoopCurrent) == 1;
    //@ assert OK: \at(z,LoopCurrent) == \at(z,Pre);
  }
}

/*
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make at"
End:
*/
