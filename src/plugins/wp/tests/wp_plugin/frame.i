// Everything FRAMED goals should be discharged. Not the KO ones.

int g ;
void f(void) { return; }

/*@
  ensures FRAMED: \result == \old(g) ; 
  ensures KO: \result == g ;
*/
int local(void)
{
  int x = g ;
  f();
  return x;
}

//@ensures KO: \result == \old(g) ;
int global(void)
{
  f();
  return g;
}

//@ensures KO: \result == \old(*p) ;
int localref(int *p)
{
  f();
  //@ assert FRAMED: p == \at(p,Pre);
  return *p;
}

//@ensures KO: \result == r ;
int alias(int r)
{
  int p = (int) &r ;
  f();
  return r ;
}
