/* run.config
   STDOPT: +"-wp-no-extensional"
*/
/* run.config_qualif
   COMMENT:
   STDOPT: +"-wp-no-extensional"
*/

int A[20] = {10,11,12} ;

/*@ 
  requires OK: A[1]==11 && A[19]==0 ; 
 */
int main(void) ;


/*@ 
  requires Init: A == \at( A , Init );
  ensures OK: \result == 12 ;
*/
int job(void) { return A[2]; }

/*@ ensures OK: \at( A[8] , Init ) == 0 ; */
void foreign(void) { return ; }

/*@ ensures KO: \result == 12 ; */
int extra(void) { return A[2]; }
