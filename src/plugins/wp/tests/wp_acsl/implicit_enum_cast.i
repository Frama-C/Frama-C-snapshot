typedef enum CODE { OK=1, KO=2 } err;

//@ensures (\result==OK) <==> (x>0); assigns \nothing;
err foo(int x);

//@ensures (\result==OK) <==> (x>0 && y>0); assigns \nothing;
err bar(int x,int y)
{
  err s = OK;
  if (foo(x) != OK) s = KO;
  if (foo(y) != OK) s = KO;
  return s;
}
