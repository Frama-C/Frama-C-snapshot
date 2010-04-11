/*@ lemma mean_1 : \forall integer x, y; x <= y ==> x <= (x+y)/2 <= y; */

/*@ predicate sorted{L}(int t[],integer n) =
  @   \forall integer i, j;
  @        0 <= i <= j <= n ==> t[i] <= t[j] ;
  @*/


/* bsearch(t,n,v) search for element v in array t
   between index 0 and n-1
   array t is assumed sorted in increasing order
   returns an index i between 0 and n-1 where t[i] equals v,
   or -1 if no element of t is equal to v
 */

/*@ requires
  @   n >= 0 && \valid_range(t,0,n-1) && sorted((int[])t,n-1);
  @ behavior search_success:
  @   ensures \result >= 0 ==> t[\result] == v;
  @ behavior search_failure:
  @   ensures \result < 0 ==>
  @     \forall integer k; 0 <= k < n ==> t[k] != v;
  @*/
int bsearch(int* t, int n, int v) {
  int l = 0, u = n-1;
  /*@ loop invariant
    @   0 <= l && u <= n-1 &&
    @   \forall int k; 0 <= k && k < n ==> t[k] == v ==> l <= k && k <= u;
    @ loop variant u-l;
    @*/
  while (l <= u ) {
    int m = (l + u) / 2;
    if (t[m] < v) l = m + 1;
    else if (t[m] > v) u = m - 1;
    else return m;
  }
  return -1;
}

/*
Local Variables:
compile-command: "../../bin/toplevel.opt -jessie bsearch.c"
End:
*/
