/*@ requires \valid(p);
  @ assigns *p;
  @ ensures *p==n;
  @*/
int f(int *p,int n) {
  *p = n;
  return 0; //NdV not sure it is correct, but the return statelent was
            // missing anyway
}

int t[10];

/* post-condition should be trivially established
 * if a separation analysis is able to separate
 * t[0..4] and t[5..9]
 */
/*@ ensures t[0]==0;
  */
int main() {
  int i;
  /*@ loop invariant 0 <= i && i <= 5 &&
    @   \forall int j; 0 <= j && j < i ==> t[j]==0;
    @*/
  for(i=0; i<5; i++) {
    f(t+i,0);
  }
  /*@ loop invariant 5 <= i && i <= 10 &&
    @   \forall int j; 5 <= j && j < i ==> t[j]==1;
    @ loop assigns t[5..9];  // needed when separation analysis too weak
    @*/
  for(i=5; i<10; i++) {
    f(t+i,1);
  }
  return 0;
}
