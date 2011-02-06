/*@
   predicate is_valid_int_range(int* p, int n) =
           (0 <= n) && \valid_range(p,0,n-1);

   lemma foo: \forall int* p,n; is_valid_int_range(p,n) <==> \valid_range(p,0,n-1);

*/
/*@
   predicate
     found{A}(int* a, int n, int val) =
       \exists int i; 0 <= i < n && a[i] == val;
*/
/*@
   predicate
     found_first_of{A}(int* a, int m, int* b, int n) =
       \exists int i; 0 <= i < m && found{A}(b, n, \at(a[i],A));
*/

/*@
   requires is_valid_int_range(a, n);

   assigns a[0..n-1];

   ensures \forall int i; 0 <= i < n ==> a[i] == val;
*/
void fill(int* a, int n, int val)
{
  /*@
  loop invariant 0 <= i <= n;
  loop invariant \forall int k; 0 <= k < i ==> a[k] == val;
  loop   variant n-i;
  */
  for (int i = 0; i < n; i++)
     a[i] = val;
}

int main (void) { return 0 ; }
