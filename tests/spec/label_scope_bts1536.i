void f() {
  int i;
 LInit:

  {
  LLoop:
    i = 0;
    //@ assert \at(1,LInit) == 1;
  }
}

int x;
void g(void) {
 L1: L2: //@ assert \at(x,L1) == \at(x,L2);
 L3: L4: ;

  //@ assert \at(x,L1) == \at(x,L2);
  //@ assert \at(x,L3) == \at(x,L4);
  return ;
}

void h(void) {
 L1: L2: //@ ensures \at(x,L1) == \at(x,L2);
 L3: L4: ;

  //@ assert \at(x,L1) == \at(x,L2);
  //@ assert \at(x,L3) == \at(x,L4);
  return ;
}
