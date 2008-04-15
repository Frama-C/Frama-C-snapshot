
//@ requires \valid(x);
void zero(int* x) {
  char *c = (char*)x;
  *c = 0;
  c++;
  *c = 0;
  c++;
  *c = 0;
  c++;
  *c = 0;
}

/* 
Local Variables:
compile-command: "LC_ALL=C make simple_cast"
End:
*/
