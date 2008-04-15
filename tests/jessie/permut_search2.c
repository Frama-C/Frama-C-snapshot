/*@ axiomatic PermutSize {
  @  logic int permut_size{Here}(int *arr); 
  @    // reads arr[0..];
  @  axiom permut_valid{Here}:
  @    \forall int *arr; 
  @      0 <= permut_size(arr) ==> \valid_range(arr,0,permut_size(arr));
  @  axiom permut_bound{Here}:
  @    \forall integer i; \forall int *arr; 
  @      0 <= i <= permut_size(arr) ==> 0 <= arr[i] <= permut_size(arr);
  @ }
  @*/

//@ requires 0 <= permut_size(arr);
int permut_search(int *arr, int v) {
  int idx = 0;
  //@ loop invariant 0 <= idx <= permut_size(arr);
  while (1) {
    if (arr[idx] == v) return idx;
    idx = arr[idx];
  }
} 
