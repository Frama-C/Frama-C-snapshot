
int find(const int* a, int n, int val)
{
  /*@
    loop assigns i;
    loop invariant 0 <= i <= n;
    loop variant n-i;
    loop invariant \forall int k; 0 <= k < i ==> a[k] != val;
   */
  for (int i = 0; i < n; i++)
    if (a[i] == val)
      return i;

  return n;
}

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
   requires is_valid_int_range(a, m);
   requires is_valid_int_range(b, n);

   assigns \nothing;

   behavior found:
     assumes found_first_of(a, m, b, n);
     ensures 0 <= \result < m;
     ensures found(b, n, a[\result]);
     ensures !found_first_of(a, \result, b, n);

   behavior not_found:
    assumes !found_first_of(a, m, b, n);
     ensures \result == m;

   complete behaviors found, not_found;
   disjoint behaviors found, not_found;
*/
int find_first_of(const int* a, int m, const int* b, int n)
{
  /*@
     loop invariant 0 <= i <= m;
     loop variant m-i;
     loop invariant !found_first_of(a, i, b, n);
  */
  for(int i = 0; i < m; i++)
     if (find(b, n, a[i]) < n)
          return i;

  return m;
}
int main (void) { return 0 ; }
