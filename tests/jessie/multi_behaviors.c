int x;

/*@ ensures 1 <= \old(x) <= 2 ==> x == 2;
  @ behavior one:
  @   assumes x == 1;
  @ behavior two:
  @   assumes x == 2;
  @*/
void f(int z) {
  //@ for one, two: assert x == 1 || x == 2;
  if (x == 1)
    //@ for one: assert x == 1;
    x++;
  ;
  //@ for one, two: assert x == 2;
}

/*
Local Variables:
compile-command: "LC_ALL=C make multi_behaviors"
End:
*/
