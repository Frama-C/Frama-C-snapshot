void f() { 
  //@ assert 0==0;
  int x;
  //@ ensures x==3;
  int y = x = 3;
  x = 0;
  y = 1;
  return;
}
