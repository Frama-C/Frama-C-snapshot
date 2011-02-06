/*@
   predicate is_valid_int_range(int* p, int n) =
           (0 <= n) && \valid_range(p,0,n-1);

   lemma foo: \forall int* p,n; is_valid_int_range(p,n) <==> \valid_range(p,0,n-1);
*/

/*@
   requires \valid(p);
   requires \valid(q);
   requires \separated(p,q);

   assigns *p;
   assigns *q;

   ensures *p == \old(*q);
   ensures *q == \old(*p);
*/
void swap(int* p, int* q)
{
  int const save = *p;
  p++;p--;
  *p = *q;
  *q = save;
}



/*@
   requires is_valid_int_range(a, n);
   requires is_valid_int_range(b, n);
   //requires \separated(a, b);

   assigns a[0..n-1];
   assigns b[0..n-1];

   ensures \forall int k; 0 <= k < n ==> a[k] == \old(b[k]);
   ensures \forall int k; 0 <= k < n ==> b[k] == \old(a[k]);
*/
void swap_ranges(int* a, int n, int* b)
{
  /*@
     loop assigns a[0..i-1];
     loop assigns b[0..i-1];

     loop invariant 0 <= i <= n;
     loop invariant \forall int k; 0 <= k < i ==>
                     a[k] == \at(b[k],Pre);
     loop invariant \forall int k; 0 <= k < i ==>
                     b[k] == \at(a[k],Pre);

     loop   variant n-i;
  */
  for (int i = 0; i < n; i++)
     swap(&a[i], &b[i]);
}
int main (void) { return 0 ; }
