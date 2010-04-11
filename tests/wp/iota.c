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
   requires val + n < ((1<<31)-1); // INT_MAX;

   assigns a[0..n-1];

   ensures \forall int k; 0 <= k < n ==> a[k] == val + k;
*/
void iota(int* a, int n, int val)
{
  /*@
     loop assigns a[0..i-1];
     loop invariant 0 <= i <= n;
     loop   variant n-i;
     loop invariant \forall int k; 0 <= k < i ==> a[k] == val+k;
  */
  for(int i = 0; i < n; ++i)
     a[i] = val + i;
}
int main (void) { return 0 ; }
