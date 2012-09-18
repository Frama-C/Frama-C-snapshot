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

/*
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make at"
End:
*/
