/*@ requires \exists int n; 0 <= n && \valid_range(arr,0,n) && 
  @                    \forall int i; 0 <= i <= n ==> 0 <= arr[i] <= n;
  @*/
int permut_search(int *arr, int v) {
  int idx = 0;
  /*@ loop invariant \exists int n; \valid_range(arr,0,n) && 
    @                    0 <= idx <= n &&
    @                    \forall int i; 0 <= i <= n ==> 0 <= arr[i] <= n;
    @*/
  while (1) {
    if (arr[idx] == v) return idx;
    idx = arr[idx];
  }
} 
