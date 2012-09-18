/* run.config
   DONTRUN: main test is in bts0672_link.c
 */

#ifdef PROTO
int Frama_C_nondet(int a, int b);
#endif

void main () {
  int x =  Frama_C_nondet(0,59);
}
