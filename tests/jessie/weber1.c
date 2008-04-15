
/*@ predicate disjoint_arrays(int *a, int *b, integer i) =
  @   \forall integer k1, k2;
  @      0 <= k1 < i && 0 <= k2 < i ==> a + k1 != b + k2;
  @*/

/*@ requires 0 < n;
  @ requires \valid_range(a, 0, n-1) && \valid_range(b, 0, n-1);
  @ requires disjoint_arrays(a, b, n);
  @ ensures  \forall int k; 0 <= k < n ==> a[k] == \at(b[k],Old);
  @*/
void array_cpy(int* a, int n, int* b)
{
   /*@ loop invariant 0 <= i <= n;
     @ loop invariant \forall int m; 0 <= m < n  ==> b[m] == \at(b[m],Pre);
     @ loop invariant \forall int m; 0 <= m < i  ==> a[m] == b[m];
     @*/
   for (int i = 0; i < n; i++)
     a[i] = b[i];
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j weber1"
End:
*/
