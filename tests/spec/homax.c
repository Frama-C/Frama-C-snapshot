/*@ requires n > 0 &&\valid(p + (0..n-1));
    ensures \result == \max(0,n-1,\lambda integer i; p[i]);
*/
int max_seq(int* p, int n);
int max_seq(int* p, int n) {
  int res = *p;
  for(int i = 0; i < n; i++) {
    if (res < *p) { res = *p; }
    p++;
  }
  return res;
}
