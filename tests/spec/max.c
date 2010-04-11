/* various specification of max function
 */

/*@ axiomatic IsMax {
  @  predicate is_max{L}(integer max, int t[], integer length);
  @     // reads t[..];
  @  axiom max_gt{L}:
  @    \forall int t[], integer max, length, i;
  @      is_max(max,t,length) ==> 0 <= i < length ==> t[i] <= max;
  @  axiom max_eq{L}:
  @    \forall int t[], integer max, length;
  @    is_max(max, t, length) ==> \exists integer i; t[i] == max;
  @ }
  @*/

/*@ requires \valid_range(t,0,n-1);
  @ behavior nonempty:
  @    assumes n > 0;
  @    ensures 0<= \result < n &&
  @            (\forall int i; 0 <= i < n ==> t[\result] >= t[i]) &&
  @            is_max(t[\result],(int[])t,n);
  @  behavior empty:
  @    assumes n <= 0;
  @    ensures \result == -1;
  @*/
int max(int t[], int n) {
  int imax = 0, i;
  /*@ ghost int max; */
  if (n<=0) return -1;
  /*@ ghost max = t[0]; */
  /*@ loop invariant
    (\forall int j; 0<= j < i ==> t[imax] >= t[j]) &&
    is_max(max,(int[])t,i-1);
  */
  for(i = 1; i < n; i++) {
    if (t[i] > t[imax]) {
      imax = i;
      /*@ ghost max = t[i]; */
    }
  }
  return imax;
}

int main() {
  int test [] = { 1, 2, 3, 4, 9, 8, 7, 6, 5, 10};
  /*@ assert \valid_range(test,0,9); */
  int x = max(test,10);
  /*@ assert test[x] >= 10; */
  return 0;
}
