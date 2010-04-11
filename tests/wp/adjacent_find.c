
/* run.config
   OPT:  -journal-disable -wp -wp-model Store -wp-proof none
   OPT:  -journal-disable -wp -wp-model Hoare -wp-proof none
*/ 

/* run.config_ergo
   OPT:  -journal-disable -wp -wp-model Store -wp-proof alt-ergo
*/

/* run.config_z3
   OPT:  -journal-disable -wp -wp-model Store -wp-proof z3
*/

/* run.config_simplify
   OPT:  -journal-disable -wp -wp-model Store -wp-proof simplify
*/


/* 
   kind : Positive
   model name : Store ; bhv : Provable
   model name : Hoare ; bhv : Out of Scope 
 */


/*@
   predicate is_valid_int_range(int* p, int n) =
           (0 <= n) && \valid_range(p,0,n-1);

   lemma foo: \forall int* p,n; is_valid_int_range(p,n) <==> \valid_range(p,0,n-1);

*/

/*@
   predicate
     adjacent_found{Label}(int* a, int n) =
       \exists int i; 0 <= i < n-1 && a[i] == a[i+1];
*/

/*@
   requires is_valid_int_range(a, n);

   assigns \nothing;

   behavior some:
     assumes adjacent_found(a, n);
     ensures 0 <= \result < n-1;
     ensures a[\result] == a[\result+1];
     ensures !adjacent_found(a, \result);

   behavior none:
     assumes !adjacent_found(a, n);
     ensures \result == n;

   complete behaviors some, none;
   disjoint behaviors some, none;
*/
int adjacent_find(int* a, int n)
{
  if (0 == n) return n;

  /*@
     loop assigns \nothing;
     loop invariant 0 <= i < n;
     loop   variant n-i;
     loop invariant !adjacent_found(a, i);
     loop invariant 0 < i ==> a[i-1] != a[i];
  */
  for (int i = 0; i < n-1; i++)
     if (a[i] == a[i+1])
       return i;

  return n;
}

int main (void) {return 0;}
