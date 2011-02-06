void f () {
  int x = 0;
 L:
  x++;
  /*@ ensures \at(\true,Pre); */
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

//@ logic integer foo{L}(integer x) = x+1;

//@ assigns x[0..foo(0)];
int u(int *x) {
  *(x++)=0;
  *x = 1;
  /*@ assert \at(\true,Pre); */
  return *x;
}

int X;

void labels_in_stmt_annot (void) {
  X ++;
  /*@ requires X > \at(X, Pre);
      ensures X == \old(X) + 1;
      ensures X == \at(X,Pre) + 2;
      ensures X == \at(X,Post);
  */
  X++;
  //@ ensures X == \at(X,Here);
  X++;
  //@ assert X == \at(X,Pre) + 3;
}
