#pragma JessieIntegerModel(math)

#include "sorting.h"

/*@ requires \valid(t+i) && \valid(t+j);
  @ assigns t[i],t[j];
  @ ensures Swap{Old,Here}(t,i,j);
  @*/
void swap(int t[], int i, int j) {
  int tmp = t[i];
  t[i] = t[j];
  t[j] = tmp;
}
    
/*@ requires \valid_range(t,0,n-1);
  @ behavior sorted:
  @   ensures Sorted(t,0,n-1);
  @ behavior permutation:
  @   ensures Permut{Old,Here}(t,0,n-1);
  @*/
void min_sort(int t[], int n) {
  int i,j;
  int mi,mv;
  if (n <= 0) return;
  /*@ loop invariant 0 <= i < n;
    @ for sorted: 
    @  loop invariant 
    @   Sorted(t,0,i) && 
    @   (\forall integer k1, k2 ; 
    @      0 <= k1 < i <= k2 < n ==> t[k1] <= t[k2]) ;
    @ for permutation:
    @  loop invariant Permut{Pre,Here}(t,0,n-1);
    @ loop variant n-i;
    @*/
  for (i=0; i<n-1; i++) {
    // look for minimum value among t[i..n-1]
    mv = t[i]; mi = i;
    /*@ loop invariant i < j && i <= mi < n;
      @ for sorted: 
      @  loop invariant
      @    mv == t[mi] &&
      @    (\forall integer k; i <= k < j ==> t[k] >= mv);
      @ for permutation:
      @  loop invariant
      @   Permut{Pre,Here}(t,0,n-1);
      @ loop variant n-j;
      @*/
    for (j=i+1; j < n; j++) {
      if (t[j] < mv) { 
	mi = j ; mv = t[j]; 
      }
    }
    swap(t,i,mi);
  }
}

