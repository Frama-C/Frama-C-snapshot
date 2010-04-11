/* run.config
   OPT:  -journal-disable -wp -wp-model Store -wp-proof none
   OPT:  -journal-disable -wp -wp-model Hoare -wp-proof none
*/ 

/* run.config_ergo
   OPT:  -journal-disable -wp -wp-model Store -wp-proof alt-ergo
*/

/* run.config_z3
   OPT:  -journal-disable -wp -wp-model Store -wp-proof z3
*/

/* run.config_simplify
   OPT:  -journal-disable -wp -wp-model Store -wp-proof simplify
*/


/* 
   kind : Positive
   model name : Store ; bhv : Provable
   model name : Hoare ; bhv : Out of Scope
 */




struct Tb { int b ; } ;
struct Ta {
  struct Tb *a ;
}  x ;


/*@ requires \valid((x.a+i)) ;
  ensures x.a[i].b == v; 
  ensures  (*((x.a)+i)).b ==  x.a[i].b ; */
int main (int i, int v) {
  (*((x.a)+i)).b = v+1 ;
  x.a[i].b = v;
  return 1;
}



int *p;
/*@ ensures *p == 0; 
*/
void main2 () {
  int tmp;
  *p = 0;
  tmp=1;
}

int j;
//@ ensures *\result == 4;
int *  result_offset_val (void)
{
  int *p; 
  p = &j; 
  j=4;
  return p;
}


