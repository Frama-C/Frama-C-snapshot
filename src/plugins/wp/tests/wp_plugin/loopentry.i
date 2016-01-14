
/*@ requires (n+p) > 0; */
void f(int n,int p)
{
  int r;
  n += p;
  r = 0;
  /*@ 
    loop invariant \at(n,LoopEntry) == \at(n+p,Pre);
    loop invariant n + r == \at(n,LoopEntry);
  */
  while (n--) r++;
}
