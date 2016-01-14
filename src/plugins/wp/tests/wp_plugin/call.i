/* run.config_qualif
   DONTRUN: (config_qualif) Nothing to improve here
*/

/* run.config_qed
   DONTRUN: (config_qed) see config_qualif
*/

/* The objective here is to check that 'job' preconditions are 
   only proved under default behavior (not for A nor B).
*/

/*@ requires x > 0 ; ensures \result > 0; assigns \nothing ; */
int job(int x);


/*@ ensures \result >= 0 ;
  @ assigns \nothing ;
  @ behavior A:
  @ assumes x < y;
  @ ensures \result > 0 ;
  @ behavior B:
  @ assumes x > y;
  @ ensures \result > 0 ;
*/
int main(int x,int y)
{
  if (x<y) return job(y-x);
  if (x>y) return job(x-y);
  return 0;
}
