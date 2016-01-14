/* run.config
   OPT: -wp-no-core
   OPT: -wp-core
*/

/* run.config_qualif
   DONTRUN:
*/

int a,b,c,x;
//@ predicate OBS(integer x,integer y);
//@ ensures OBS(\old(x),x);
void f(void)
{
  if (a)
    //@ ensures x == \old(x);
    a++;
  if (b) 
    //@ ensures x == \old(x);
    b++;
  x++;
  if (c)
    //@ ensures x == \old(x);
    c++;
}
