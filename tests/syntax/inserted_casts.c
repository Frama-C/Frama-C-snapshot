/* run.config
   STDOPT: +"-load-script tests/syntax/inserted_casts"
*/
int f(int b)
{
    int r;
	if (b*b != 0)
        r=0;
	else r=-1;
    return r;
}

int g(int a)
{
  unsigned int r;
  r = a + 3;
  a *= r;
  return (a - r);
}
