/*@ predicate pcond1(integer p) = p > 0;
  @ predicate pcond2(integer p) = p < 10;
  @*/

/*@ behavior ok:
  @   assumes pcond1(x) && pcond2(y);
  @   ensures \result == 1;
  @ behavior ko:
  @   assumes !pcond1(x) || !pcond2(y);
  @   ensures \result == 0;
  @*/
int ftest2(int x, int y)
{
 return (x>0 && y<10);
}

/* 
Local Variables:
compile-command: "LC_ALL=C make duprat3"
End:
*/
