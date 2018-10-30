/* run.config_qualif
   OPT: -wp-steps 50
*/

int a[5];

/*@
  ensures \result == a ;
  assigns \result[0..n];
*/
int * job(int n);

void correct(void)
{
  int * p = job(2);
  //@ assert OK: p[3] == \at(a[3],Pre) ;
}

void wrong(void)
{
  int * p = job(2);
  //@ assert KO: p[1] == \at(a[1],Pre) ;
}
