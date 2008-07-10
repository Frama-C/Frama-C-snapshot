int x;

/*@ behavior one:
  @   assumes x == 1;
  @   ensures x == 10;
  @*/
void f(int z) {
  if (z)
    //@ for one: loop invariant 1 <= x <= 10;
    while (x < 10) x++;
  else
    //@ for one: assert x == 1;
    x = 10;
}

/*
Local Variables:
compile-command: "LC_ALL=C make behavior"
End:
*/
