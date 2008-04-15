/*@
  @requires last >= first;
  @requires \valid_range(first, 0, last-first-1);
  @behavior is_not_empty:  
  @  assumes last > first;
  @  ensures  \forall integer i; 0 <= i < last-first ==> first [i] == value;
  @behavior is_empty:
  @  assumes last == first;
  @  ensures last == first; 
  @  
*/
void fill_int_array (int* first, int* last,  int value )
{
  //@ ghost int* a = first; 
  /*@
    @loop invariant a <= first <= last;
    @loop invariant \forall integer k; 0 <= k < first-a ==> a[k] == value;
  */
  while (first != last)  *first++ = value;
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j weber3.nosep"
End:
*/
