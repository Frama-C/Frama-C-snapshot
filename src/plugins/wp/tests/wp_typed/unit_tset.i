/* run.config_qualif
   OPT:
*/

/*@ 
  requires \valid( p + (0..n-1) ); 
  assigns p[0..n-1];
 */
void job(int *p,int n);

typedef struct S {
  int size ;
  int value[50] ;
} ;

/*@ requires s.size < 50; */
void complex(struct S s)
{
  /*@ assigns s.value[1..s.size]; */
  job( & s.value[1] , s.size );
}
