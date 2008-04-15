/*@ requires \base_addr(q) != p;
  requires \separated(p,q);
  requires \valid(p+(0..(n-1)));
  requires \valid(q+(0..(n-1)));
  assigns p[0..n-1];
*/
void put(char* p, char* q, int n) {
  for(int i = 0; i<n; i++) /*@ assert \separated(p,q); */ *p++=*q++;
}
