/*
  run.config
  OPT: -wp-proof none -wp-print -wp-prop A 
  OPT: -wp-proof none -wp-print -wp-prop A,B

 */




//@ predicate P(integer i) = i >= 0 ; 

int y;

/*@
 requires P(x); 
 assigns \nothing; 
 ensures A:P(y);
 ensures B:P(z);
 ensures P(y+z);
*/
 int f(int x, int z)
 { return z; }
 
