/* run.config_qualif
   OPT:
   OPT: -wp-model +ref
 */

/*@ requires \valid(a) ;
  @ requires \valid(b) ;
  @ ensures A: *a == \old(*b) ;
  @ ensures B: *b == \old(*a) ;
  @ assigns E: *a,*b ;
  @*/

void swap(int *a,int *b)
{
  int tmp = *a ;
  *a = *b ;
  *b = tmp ;
  return ;
}


void main(int a,int b)
{
  if (a >= b) swap(&a,&b) ;
  //@ assert a <= b ;
}
