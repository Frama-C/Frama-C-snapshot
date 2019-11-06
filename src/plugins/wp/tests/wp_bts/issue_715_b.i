//@ predicate isValid(int *s) = \valid(s);

/*@
  requires isValid(dest);
  requires dest[0] >= 0;
*/
void dummy(int *dest);

void foo(){
  int p[1] = { 0 } ;
  dummy(p);
}
