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
   predicate
     adjacent_found{Label}(int* a, int n) =
       \exists int i; 0 <= i < n-1 && a[i] == a[i+1];
*/

/*@
   requires is_valid_int_range(a, n);

   assigns \nothing;

   behavior empty:
     assumes n == 0;
     ensures \result == 0;

   behavior not_empty:
     assumes 0 < n;
     ensures 0 <= \result < n;
     ensures \forall int i; 0 <= i < n       ==> a[\result] <= a[i];
     ensures \forall int i; 0 <= i < \result ==> a[\result] < a[i];
*/
int min_element(int* a, int n)
{
  if (0 == n) return n;

  int min = 0;
  /*@
     loop invariant 0 <= i   <= n;
     loop invariant 0 <= min <  n;
     loop invariant \forall int k; 0 <= k < i ==> a[min] <= a[k];
     loop invariant \forall int k; 0 <= k < min ==> a[min] < a[k];
     loop variant n-i;
  */
  for (int i = 0; i < n; i++)
     if (a[i] < a[min])
       min = i;

  return min;
}
int main (void) { return 0 ; }
