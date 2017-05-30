/* run.config_qualif
   COMMENT:
*/

int x,y; 

/*@ requires \valid(&x); 
    assigns \nothing ;
    ensures qed_ok: &z == &y ==> \result == x;
*/ 
int f (int z) 
{ if (&z == &y) return x ;  return 0; } 
