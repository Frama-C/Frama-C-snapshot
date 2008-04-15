/* run.config
   DONTRUN: cast not supported yet
*/

#pragma SeparationPolicy(Regions)

/*@ axiom little_endian_low_byte_short{L}: 
  @   \forall short *s; *(char*)s == *s % 256;
  @   
  @ axiom little_endian_high_byte_short{L}: 
  @   \forall short *s; *((char*)s+1) == *s / 256;
  @*/

/*@ axiom div_modulo:
  @   \forall integer i; i == 256 * (i / 256) + i % 256;
  @*/

/*@ requires \valid_range(x,0,1);
  @ ensures x[0] == \old(x[1]) && x[1] == \old(x[0]);
  @*/
void swap(char *x) {
  char tmp = *x;
  x[0] = x[1];
  x[1] = tmp;
}

/*@ requires \valid(s);
  @ ensures *s == 256 * (\old(*s) % 256) + (\old(*s) / 256);
  @*/
void reverse_endian(short *s) {
  char *c = (char*)s;
  swap(c);
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j reverse_endian3"
End:
*/
