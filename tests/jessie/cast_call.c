/* run.config
   DONTRUN: cast no working yet
*/

#pragma SeparationPolicy(Regions)

/*@ requires \valid(c);
  @ ensures \result == c;
  @*/
char* f(char *c) {
  return c;
}  
  
void g() {
  int i;
  char *c = f((char*)&i);
  *c = 0;
  //@assert i == 0;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make -j cast_call"
End:
*/
