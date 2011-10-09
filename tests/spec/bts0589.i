int x;

int g(void)
{ int a;
  //@ assigns a,x ;
  a = x++ ;
  return a;
}

