/* run.config
   DONTRUN: not robust enough + require native dynlink
   OPT: -load-script tests/misc/cabscond_script.ml -print
*/
/* run.config_cabscond
   OPT: -load-script tests/misc/cabscond_script.ml -print
*/

// Tests with:
// ptests tests/misc/cabscond.c -config cabscond

int f(int);
int test(int a,int b,int c)
{

  if ( (f(a) && !f(b)) || f(c) )
    { return 0; }
  else
    { return 1; }

  if ( (f(a) && !f(a)) || f(a) )
    { return 0; }
  else
    { return 1; }

  while ( (f(a) && !f(a)) || f(c) )
    { return 0; }

  for ( a=0 ; (f(a) && !f(b)) || f(c) ; a+=c )
    { return 0; }

}
