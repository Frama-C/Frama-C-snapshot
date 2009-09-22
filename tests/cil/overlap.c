/* run.config 
   OPT:-print
*/
int main() {
  int n = 8 ;
  int z[8] ;
  for (int i=0;i<n;) {
  first:
    z[i++] = z[i++]+(i--) ;
  last:
    z[i++] = z[i++]+(i--) ;
    //@ assert \at( 0<=i<n , first ) ;
    //@ assert \at( 0<=i<n , last ) ;
  }
  return z[n-1];
}
