/* run.config
   EXECNOW: make -s tests/misc/inserted_casts_plugin/inserted_casts.cmxs
   CMD: bin/toplevel.opt
   OPT: -print -load-module tests/misc/inserted_casts_plugin/inserted_casts.cmxs
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
