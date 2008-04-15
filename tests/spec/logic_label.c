void f () {
  int x = 0;
 L:
  x++;
  x++;
  /*@ assert \at(x,L) == 0; */
  /*@ assert \at(x==0,L); */
}

int g(int i) {
 lab:
  //@ assert i == \at(i,lab);
  return i;
}

//@ predicate modified{L1,L2}(int x) = \at(x,L1)!=\at(x,L2);

//@ logic integer diff{L1,L2}(integer x) = \at(x,L1) - \at(x,L2);

int h() {
  int x = 0;
 l:
  x++;
  //@ assert modified{Here,l}(x) && diff{Here,l}(x) == 1;
  return 0;
}
