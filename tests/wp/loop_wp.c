/*@  ensures \result == 10 ; */
int main () {
  int i;
  int G = -1;

  /*@ loop invariant G + 1 == i ; */
  for (i=0; i<= 10; i++) G = i ;
  
  return G;
}
