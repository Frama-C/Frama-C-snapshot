/*@ predicate disjoint_arrays(int* a, int* b, integer i) =
     \forall integer k1, k2;
        0 <= k1 < i && 0 <= k2 < i ==> a + k1 != b + k2;
*/
/*@
 requires last > first;
 requires disjoint_arrays(first, result, last-first); 
 
 requires \valid_range  (first, 0, last-first-1);
 requires \valid_range  (result, 0, last-first-1);
 ensures  \forall integer i; 0 <= i < last-first ==> result[i] == first[i];  
*/
 
int* copy_int_array (int* first, int* last, int* result)
{
 //@ ghost int* a = first;
 //@ ghost int* b = result;
 //@ ghost int length = last-first;
 
    /*@
     loop invariant a <= first <= last;
     loop invariant result - b == first - a;
     loop invariant b <= result <= b+length;     
     loop invariant \forall integer k; 0 <= k < first-a ==> a[k] == b[k];
    */
  while (first!=last) *result++ = *first++;
  return result;
}
