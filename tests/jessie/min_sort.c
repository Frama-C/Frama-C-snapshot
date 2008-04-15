



#include "sorting.h"

/*@ requires \valid(t+i) && \valid(t+j);
  @ ensures Swap{Old,Here}(t,i,j);
  @*/
void swap(int t[], int i, int j);

/*@ requires n >= 0 && \valid_range(t,0,n-1);
  @ behavior sorted:
  @   ensures Sorted(t,0,n-1);
  @ behavior permutation:
  @   ensures Permut{Old,Here}(t,0,n-1);
  @*/
void min_sort(int t[],int n) {
  int i,j;
  int mi,mv;
  for (i=0; i<n-1; i++) {
    // look for minimum value among t[i..n-1]
    mv = t[i]; mi = i;
    for (j=i+1; j < n; j++) {
      if (t[j] < mv) { 
	mi = j ; mv = t[j]; 
      }
    }
    swap(t,i,mi);
  }
}
