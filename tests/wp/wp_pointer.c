/*@ requires \valid(p+1) ;
  ensures *(p+1) == 56 ; */
int main(int *p) {
  int x = 55;
  *(p+1) = x + 1 ;
  //@ assert 56 == *(p+1) ;
  x++;
  return 0;
}
