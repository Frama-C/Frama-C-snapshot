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
   requires is_valid_int_range(b, n);

   assigns b[0 .. n-1];

   ensures \forall int j; 0 <= j < n ==>
            a[j] == old_val && b[j] == new_val ||
            a[j] != old_val && b[j] == a[j];
   ensures \result == n;
*/
int replace_copy(const int* a, int n, int* b, int old_val, int
    new_val)
{
  /*@
     loop assigns b[0..i-1];
     loop invariant 0 <= i <= n;
     loop   variant n-i;
     loop invariant \forall int j; 0 <= j < i ==>
                     a[j] == old_val && b[j] == new_val ||
                     a[j] != old_val && b[j] == a[j];
  */
  for (int i = 0; i < n; ++i)
     b[i] = (a[i] == old_val ? new_val : a[i]);

  return n;
}
int main (void) { return 0 ; }
