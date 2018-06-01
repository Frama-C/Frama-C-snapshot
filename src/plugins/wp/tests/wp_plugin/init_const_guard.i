/* run.config
   OPT: -wp-init-const
*/

/* run.config_qualif
   OPT: -wp-init-const
*/

int x ;
int * const q = &x ;
const int * p = &x ;

/*@  
  ensures Const: q == &x ;
  ensures Pointed_Valid: \valid(q);
  ensures Q_ReadOnly: \valid_read(&q);
  ensures Q_NotWrite: !\valid(&q);
*/ 
int f(void)
{
  return 0;
}


/*@ 
  ensures P_not_Const: \valid(&p);
*/ 
void g(void)
{
  p = &x ;
  //@ assert Read: \valid_read(p);
  //@ assert Guard_against_Const: !\valid(p);
  *((int *)p) = 2 ;
}
