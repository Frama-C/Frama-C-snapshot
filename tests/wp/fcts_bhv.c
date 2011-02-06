/*
  run.config
  OPT: -wp-proof none -wp-print -wp-fct f,g 
  OPT: -wp-proof none -wp-print -wp-fct f,g -wp-bhv B1
  OPT: -wp-proof none -wp-print -wp-bhv B1 
  
*/

//@ predicate P (integer x)= x/2 > 0 ;
//@ predicate P1 (integer x) = x/2 > 0; 
//@ predicate P2 (integer x) = x/2 > 0;

/*@
  requires P(1); 
  assigns \nothing;
  behavior B1: 
   assumes P1(2); 
   assigns \nothing; 
   ensures P1(3); 
  behavior B2: 
   assumes P2(4); 
   assigns \nothing; 
   ensures P2(5); 
 ensures P(6);
 */
void f(void)
{ 
  int x ; x =1 ; 
 } 



/*@
  requires P(7); 
  assigns \nothing;
  behavior B1: 
   assumes P1(9); 
   assigns \nothing; 
   ensures P1(10); 
  behavior B2: 
   assumes P2(11); 
   assigns \nothing; 
   ensures P2(12); 
 ensures P(13);
 */
void g(void)
{ 
  int x ; x =1 ; 
 }

/*@
 requires P(14); 
 assigns \nothing; 
 ensures P(15);
 */
void h(void)
{ 
  int x ; x =1 ; 
 }
