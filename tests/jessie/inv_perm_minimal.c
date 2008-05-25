/*@ requires 
  @   n >= 1 && \valid_range(t,1,n) &&
  @   \forall int k; 1 <= k <= n ==> 1 <= t[k] <= n;
  @*/
void safety(int *t, int n) {
  int m = n, j = -1;
  /*@ loop invariant 
    @   1 <= m <= n && -n <= j <= -1 &&
    @   \forall int k; 1 <= k <= n ==> (-n <= t[k] <= -1 || 1 <= t[k] <= n);
    @*/
  do {
    int i = t[m];
    if (i > 0) {
      /*@ loop invariant 
	@   1 <= m <= n && 1 <= i <= n && -n <= j <= -1 &&
	@   \forall int k; 1 <= k <= n ==> (-n <= t[k] <= -1 || 1 <= t[k] <= n);
	@*/
      do {
	t[m] = j;
	j = -m;
	m = i;
	i = t[m];
      } while (i > 0);
      i = j;
    }
    t[m] = -i;
    m--;
  } while (m > 0);
}

/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make inv_perm_minimal"
End:
*/
