
/* binary_search(t,n,v) search for element v in array t 
   between index 0 and n-1
   array t is assumed sorted in increasing order
   returns an index i between 0 and n-1 where t[i] equals v, 
   or -1 if no element of t is equal to v  
 */

/* safety */


/*@ axiom mean_1 : \forall int x, int y; x <= y => x <= (x+y)/2 <= y */

/*@ requires n >= 0 && \valid_range(t,0,n-1) */
int binary_search1(int* t, int n, int v) {
  int l = 0, u = n-1;
  /*@ invariant 0 <= l && u <= n-1 
    @ variant   u-l 
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
compile-command: "make binary_search_safety.gui"
End: 
*/
