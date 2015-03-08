/* run.config
   OPT:
   OPT: -wp-model +ref
*/
/* run.config_qualif
   OPT:
   OPT: -wp-model +ref
*/

int z ;
int *p ;

void job (int x)
{
  //@ assert \valid(&x) ;
  {
    int y ;
    int z ;
    p = &y ;
    y = 4 ;
    //@ assert \valid(p) ;
    //@ assert \valid(&z) ;
  }
  //@ assert !\valid(p) ;
}

//@ assigns \nothing ;
int f(int x) { return x; }

//@ assigns \nothing ;
int g(int x) { x++; return x; }

//@ ensures !\valid(\result) ;
int * h(int x) { return &x; }
