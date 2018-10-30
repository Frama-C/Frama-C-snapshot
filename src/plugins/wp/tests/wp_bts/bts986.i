/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-steps 50
*/

void f (void) 
{
  int * p ;
  {
    int x ;
    p = &x ;
  }
  //@ assert A:!\valid(p);
}

