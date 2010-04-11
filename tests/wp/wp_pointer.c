
/*@ requires \valid(p+1) ;
  ensures *(p+1) == 56 ; */
int main(int *p) {
  int x = 55;
  *(p+1) = x + 1 ;
  //@ assert 56 == *(p+1) ;
  x++;
  return 0;
}

int G;
/*@ behavior b1 :
  @   ensures \old(G) >=0 ==> G == \old(G);
  @ behavior b2 : // this one seems similar to b1
  @   assumes G >= 0;
  @   ensures  G == \old(G);
*/
void f () {
  int * p;
  p = &G;
  if (G < 0)
    *p = 0;
}
