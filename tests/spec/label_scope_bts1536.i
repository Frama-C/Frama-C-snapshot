void f() {
  int i;
 LInit:

  {
  LLoop:
    i = 0;
    //@ assert \at(1,LInit) == 1;
  }
}
