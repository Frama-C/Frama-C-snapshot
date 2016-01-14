/* run.config
   OPT: -wp-init-const
*/

/* run.config_qualif
   OPT: -wp-init-const
*/

      int A[4] = { 1,2,3 } ;
const int B[4] = { 1,2,3 } ;

//@ ensures KO: \result == 6 ;
int fA(void) { return A[0]+A[1]+A[2]+A[3] ; }

//@ ensures OK: \result == 6 ;
int fB(void) { return B[0]+B[1]+B[2]+B[3] ; }

//@ ensures KO: \result == \at(A[3],Init) ;
int fC(void) { return A[3]; }

//@ ensures OK: \result == \at(B[3],Init) ;
int fD(void) { return B[3]; }
