/* run.config
   STDOPT: +"tests/spec/merge_2.i"
 */
/*@ requires \valid(s);
  @ assigns \nothing;
  @ ensures \result == 0 && \valid(s);
  @*/
extern int slen(const char* s);

/*@ requires x>=0; */
extern int f(int x);
