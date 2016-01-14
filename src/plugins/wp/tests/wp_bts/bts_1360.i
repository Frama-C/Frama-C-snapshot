/* run.config
   OPT: -wp-rte
*/

/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-rte
*/

/* The RTE '*_mem_access_3' obligation should fail in Function 'foo_wrong' */

/*@ requires \valid_read(a) && \valid_read(b) ;
    requires \separated(a,b);
    ensures *a == \old(*a & *b) ;
    assigns *a ;
*/
void foo_wrong(int * a,int *b)
{
  int tmp = *a & *b ;
  *a = tmp ; /* ARG ! */
}

/*@ requires \valid(a) && \valid_read(b) ;
    requires \separated(a,b);
    ensures *a == \old(*a & *b) ;
    assigns *a ;
*/
void foo_correct(int * a,int *b)
{
  int tmp = *a & *b ;
  *a = tmp ;
}
