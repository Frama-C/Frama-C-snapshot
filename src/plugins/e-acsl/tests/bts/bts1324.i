/* run.config
   COMMENT: fixed bug with typing of universal quantification
*/

/*@ behavior yes:
  @   assumes \forall int i; 0 < i < n ==> t[i-1] <= t[i];
  @   ensures \result == 1;
  @*/
int sorted(int * t, int n) {
  int b = 1;
  if(n <= 1)
    return 1;
  for(b = 1; b < n; b++) {
    if(t[b-1] > t[b])
      return 0;
  }
  return 1;
}

int main(void) {
  int t[7] = { 1, 4, 4, 5, 5, 5, 7 };
  int n = sorted(t, 7);
  /*@ assert n == 1; */
  return 0;
}
