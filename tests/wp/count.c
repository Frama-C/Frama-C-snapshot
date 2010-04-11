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
   axiomatic counting_axioms
   {
      logic integer counting{L}(int* a, integer n, int val)
        reads a[0..n-1];

      axiom counting_empty{L}:
        \forall int* a, integer n, int val; n <= 0 ==>
           counting(a, n, val) == 0;

      axiom counting_hit{L}:
        \forall int* a, integer n, int val; n >= 0 && a[n] == val ==>
           counting(a, n+1, val) == counting(a, n, val) + 1;

      axiom counting_miss{L}:
        \forall int* a, integer n, int val; n >= 0 && a[n] != val ==>
           counting(a, n+1, val) == counting(a, n, val);
    }
*/

/*@
   requires is_valid_int_range(a, n);

   assigns \nothing;

   ensures \result == counting(a, n, val);
*/
int count(const int* a, int n, int val)
{
  int cnt = 0;
  /*@
     loop invariant 0 <= i <= n;
     loop   variant n-i;
     loop invariant 0 <= cnt <= i;
     loop invariant cnt == counting(a, i, val);
  */
  for (int i = 0; i < n; i++)
     if (a[i] == val)
       cnt++;

  return cnt;
}
int main (void) { return 0 ; }
