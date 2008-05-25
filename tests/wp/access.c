/*run.config
DONTRUN: invalid pointer access. Check with Benjamin the intended behavior.
*/
struct Tb { int b ; } ;
struct Ta {
  struct Tb *a ;
}  x ;


/*@ requires \valid((x.a+i)) ;
  ensures x.a[i].b == v; 
  ensures  (*((x.a)+i)).b ==  x.a[i].b ; */
int main (int i, int v) {
  (*((x.a)+i)).b = v+1 ;
  x.a[i].b = v;
  return 1;
}
