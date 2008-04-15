/* run.config
   DONTRUN: cast not supported yet
*/

#pragma SeparationPolicy(Regions)

/*@ requires \valid(s);
  @ ensures *s == 256 * (\old(*s) % 256) + (\old(*s) / 256);
  @*/
void reverse_endian(short *s) {
  char *c = (char*)s;
  char tmp = *c;
  *c = *(c+1);
  *(c+1) = tmp;
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j reverse_endian"
End:
*/
