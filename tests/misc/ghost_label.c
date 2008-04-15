/* run.config
   DONTRUN: bug fix needed
   STDOPT: +"-jessie-analysis"
 */
int main() {
  int x = 0;
  x++;
  //@ ghost L:
  //@ assert \at(x,L) == 1;
  x++;
  return x;
}

//@ predicate p{L1,L2}(char *x);

void f(char *a, char *b, char *c, int len, int len1) {
  int k, k1;

  //@ ghost L:
  /*@ loop invariant p{L,Here}(a) && 0<=k1<=len1;
    @ loop variant len1-k1;
    @*/
  for(k1=0;k1<len1;k1++) {
    a[k1] = b[k1] * c[k1];
  }
}
