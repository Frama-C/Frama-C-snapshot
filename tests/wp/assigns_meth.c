/*
   run.config
   OPT: -wp-proof none -wp-print -wp-prop assigns -wp-assigns memory 
   OPT: -wp-proof none -wp-print -wp-prop assigns -wp-assigns effect
*/



//@ predicate P (integer i) = i >= 0;

int t [4];
int a,b ;

/*@
  requires P(a);
  assigns a , b, t[0..2] ;
  ensures P(b);
 */
void f (void){
 
  a =0 ; b =0 ; 
  t[0] = 0 ; 
  t[1] = 0; 
  t[2] = 2;
}
