/*@
  @requires \valid_range(first, 0, last-first -1) 
  @      && first <= last && \base_addr(first) == \base_addr(last);
  @behavior is_not_empty:
  @  ensures \forall integer i;
  @  0 <= i < last-first ==> first[i] == value;
*/
void fill (int* first, int* last, int value )
{
  int* it = first;

  /*@
    @loop invariant first <= it <= last 
    @            && \base_addr(first) == \base_addr(it)
    @            && \base_addr(last) == \base_addr(it);
    @loop invariant \forall integer k; 0 <= k < it - first ==> first[k] == value;
  */
  while (it != last)
    *it++ = value;
}
