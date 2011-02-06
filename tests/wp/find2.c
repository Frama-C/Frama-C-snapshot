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
   requires is_valid_int_range(a, n);

   assigns \nothing;

   behavior some:
     assumes found(a, n, val);
     ensures 0 <= \result < n;
     ensures a[\result] == val;
     ensures !found(a, \result, val);

   behavior none:
     assumes !found(a, n, val);
     ensures \result == n;

   complete behaviors some, none;
   disjoint behaviors some, none;
*/
int find2(const int* a, int n, int val)
{
  /*@
    loop invariant 0 <= i <= n;
    loop invariant !found(a, i, val);
    loop   variant n-i;
   */
  for (int i = 0; i < n; i++)
    if (a[i] == val)
      return i;

  return n;
}
int main (void) { return 0 ; }
