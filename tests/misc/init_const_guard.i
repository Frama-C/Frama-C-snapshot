/*run.config
  STDOPT: #"-main f" +"-report -then -main g -then -lib-entry -main f -then -main g"
*/

/** Same test exists in WP tests. Please keep synchronized */



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

volatile v;

/*@ 
  ensures P_not_Const: \valid(&p);
*/ 
void g(void)
{
  p = &x ;
  //@ assert Read: \valid_read(p);
  //@ assert Guard_against_Const: !\valid(p);
  if (v)
  *p = 2 ; // SHOULD BE A RUNTIME ERROR
}
