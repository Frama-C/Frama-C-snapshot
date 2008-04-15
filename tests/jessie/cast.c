
#pragma SeparationPolicy(Regions)

/*@ requires \valid(i);
  @ ensures \result == 0;
  @*/
int f(int *i) {
  char *c = (char*)i;
  *c = 0;
  c++;
  *c = 0;
  c++;
  *c = 0;
  c++;
  *c = 0;
  return *i;
}  
  
void g() {
  int i;
  i = f(&i);
  //@assert i == 0;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make -j cast"
End:
*/
