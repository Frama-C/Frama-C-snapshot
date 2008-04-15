
//@+ CheckArithOverflow = no

/*@ predicate Sorted{L}(int a[], integer l, integer h) =
  @   \forall integer i; l <= i < h ==> a[i] <= a[i+1] ;
  @*/

/*@ predicate Swap{L1,L2}(int a[], integer i, integer j) =
  @   \at(a[i],L1) == \at(a[j],L2) &&
  @   \at(a[j],L1) == \at(a[i],L2) &&
  @   \forall integer k; k != i && k != j ==> \at(a[k],L1) == \at(a[k],L2);
  @*/

/*@ axiomatic Permut {
  @  predicate Permut{L1,L2}(int a[], integer l, integer h);
  @  axiom Permut_refl{L}: 
  @   \forall int a[], integer l h; Permut{L,L}(a, l, h) ;
  @  axiom Permut_sym{L1,L2}: 
  @    \forall int a[], integer l h; 
  @      Permut{L1,L2}(a, l, h) ==> Permut{L2,L1}(a, l, h) ;
  @  axiom Permut_trans{L1,L2,L3}: 
  @    \forall int a[], integer l h; 
  @      Permut{L1,L2}(a, l, h) && Permut{L2,L3}(a, l, h) ==> 
  @        Permut{L1,L3}(a, l, h) ;
  @  axiom Permut_swap{L1,L2}: 
  @    \forall int a[], integer l h i j; 
  @       l <= i <= h && l <= j <= h && Swap{L1,L2}(a, i, j) ==> 
  @     Permut{L1,L2}(a, l, h) ;
  @ }
  @*/

class Sort {

    /*@ requires t != null && 
      @    0 <= i < t.length && 0 <= j < t.length;
      @ assigns t[i],t[j];
      @ ensures Swap{Old,Here}(t,i,j);
      @*/
    void swap(int t[], int i, int j) {
	int tmp = t[i];
	t[i] = t[j];
	t[j] = tmp;
    }
    
    /*@ requires t != null;
      @ behavior sorted:
      @   ensures Sorted(t,0,t.length-1);
      @ behavior permutation:
      @   ensures Permut{Old,Here}(t,0,t.length-1);
      @*/
    void min_sort(int t[]) {
	int i,j;
	int mi,mv;
	/*@ loop_invariant 0 <= i;
	  @ for sorted: 
	  @  loop_invariant Sorted(t,0,i) && 
	  @   (\forall integer k1 k2 ; 
	  @      0 <= k1 < i <= k2 < t.length ==> t[k1] <= t[k2]) ;
	  @ for permutation:
	  @   loop_invariant Permut{Pre,Here}(t,0,t.length-1);
	  @*/
	for (i=0; i<t.length-1; i++) {
	    // look for minimum value among t[i..n-1]
	    mv = t[i]; mi = i;
	    /*@ loop_invariant i < j && i <= mi < t.length;
	      @ for sorted:
	      @  loop_invariant mv == t[mi] &&
	      @   (\forall integer k; i <= k < j ==> t[k] >= mv);
	      @ for permutation:
	      @  loop_invariant Permut{Pre,Here}(t,0,t.length-1);
	      @*/
	    for (j=i+1; j < t.length; j++) {
		if (t[j] < mv) { 
		    mi = j ; mv = t[j]; 
		}
	    }
	    swap(t,i,mi);
	}
    }

    
}

