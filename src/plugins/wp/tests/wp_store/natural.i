/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-proof alt-ergo -wp-par 1
*/

int x,y; 

/*@ requires \valid(&x); 
    assigns \nothing ;
    ensures qed_ok: &z == &y ==> \result == x;
*/ 
int f (int z) 
{ if (&z == &y) return x ;  return 0; } 
