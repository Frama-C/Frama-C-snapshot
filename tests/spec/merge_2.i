/* run.config
 * DONTRUN: part of merge_1.i
 */
/*@ requires \valid(str2);
  @ assigns \nothing;
  @ 
  @*/
int slen(const char* str2);

/*@ 
  @ assigns \nothing;
  @ ensures \result == 0 && \valid(str);
  @*/
int slen(const char* str) {
  const char *s;
  for (s = str; *s; ++s);
  return(s - str);
}

//@ requires y>=0;
int f(int y);

int f(int z) { return z-1; }
