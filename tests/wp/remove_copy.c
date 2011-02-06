/* run.config
   DONTRUN: invalid ACSL annotations (unbound function counting)
*/

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

   ensures \forall int k; \result <= k < n ==> b[k] == \old(b[k]);
   ensures \forall int k; 0 <= k < \result ==> b[k] != val;
   ensures \forall int x; x != val ==>
            counting(a, n, x) == counting(b, \result, x);
   ensures \result == n - counting(a, n, val);
   ensures 0 <= \result <= n;
*/
int remove_copy(const int* a, int n, int* b, int val)
{
  int j = 0;
  /*@
     loop assigns b[0..j-1];

     loop invariant 0 <= j <= i <= n;
     loop invariant \forall int k; j <= k < n ==>
                      b[k] == \at(b[k],Pre);
     loop invariant \forall int k; 0 <= k < j ==> b[k] != val;
     loop invariant \forall int x; x != val ==>
                      counting(a,i,x) == counting(b,j,x);
     loop invariant j == i - counting(a,i,val);

     loop   variant n-i;
  */
  for (int i = 0; i < n; ++i)
     if (a[i] != val)
       b[j++] = a[i];
  return j;
}
