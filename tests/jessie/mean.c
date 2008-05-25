
/* right way to compute the mean of two integers without overflow
   (extracted from binary_search.c) */

//@ axiom mean_1 : \forall int x; 0 <= x ==> 0 <= x/2 <= x;

//@ requires 0 <= l <= r ;
int mean(int l, int r) {
  return l + (r - l)/2;
}

//@ requires l <= r ;
unsigned int umean(unsigned int l, unsigned int r) {
  //@ assert 0 <= (r-l)/2 <= r-l;
  return l + (r - l)/2;
}

//@ ensures (\result == x <= y) || (\result == y <= x);
int min_int(int x, int y);

//@ ensures (\result == x >= y) || (\result == y >= x);
int max_int(int x, int y);

//@ requires 0 <= x && 0 <= y;
int mean2(int x, int y) {
  int min = min_int(x,y), max = max_int(x,y);
  return min + (max - min)/2;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make mean"
End:
*/
