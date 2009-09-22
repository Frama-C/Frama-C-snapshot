/* run.config_no_native_dynlink
   EXECNOW: make -s tests/misc/inserted_casts_plugin/inserted_casts.cmo
   CMD: FRAMAC_PLUGIN=tests/misc/inserted_casts_plugin bin/toplevel.byte
   OPT: -print
*/
/* run.config
   EXECNOW: make -s tests/misc/inserted_casts_plugin/inserted_casts.cmxs
   CMD: FRAMAC_PLUGIN=tests/misc/inserted_casts_plugin bin/toplevel.opt
   OPT: -print
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
