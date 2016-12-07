/* run.config
   OPT: -wp -wp-model Caveat -wp-extern-arrays
*/

/* run.config_qualif
   OPT: -wp -wp-model Caveat -wp-extern-arrays
*/

struct S { int f ; int g ; } ;

/*@ 
  ensures \forall integer k ; 0 <= k < 10 ==> p[k].f == 1 ;
  ensures \forall integer k ; 0 <= k < 10 ==> p[k].g == 2 ;
  assigns p[0..9]; 
*/
void reset(struct S * p)
{
  /*@ 
    loop invariant 0 <= i <= 10 ;
    loop invariant \forall integer k ; 0 <= k < i ==> p[k].f == 1 ;
    loop invariant \forall integer k ; 0 <= k < i ==> p[k].g == 2 ;
    loop assigns i, p[0..9] ;
  */
  for (int i = 0 ; i < 10 ; i++) {
    p[i].f = 1 ;
    p[i].g = 2 ;
  }
}
