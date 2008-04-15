/*@
 requires last1 > first1;
 requires \valid_range  (first1, 0, last1-first1-1);
 requires \valid_range  (first2, 0, last1-first1-1);
 
 behavior equal:
  ensures  \forall integer i; 0 <= i < last1-first1 ==> first1[i] == first2[i]; 
 behavior not_equal:
  ensures  \exists integer i; 0 <= i < last1-first1 ==> first1[i] != first2[i];  
*/
int equal_int_array ( int* first1, int* last1, int* first2 )
{
//@ ghost int* a = first1, k;
/*@

loop invariant a <= first1 <= last1;

for equal:

    loop invariant \forall integer i; 0 <= k < first1-a ==> first1[k] == first2[k];

for not_equal:

    loop invariant \exists integer i; 0 <= k < first1-a ==> first1[k] != first2[k];

*/
  while ( first1!=last1 )
  {
    if (*first1 != *first2)
      return 0;
    ++first1; ++first2;
  }
  return 1;
}
