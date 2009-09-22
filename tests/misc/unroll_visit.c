/* run.config
   STDOPT: +"-print"
 */
void main() {
   /*@ loop pragma UNROLL_LOOP 2; */
  for(int i=0; i<100; i++) {
    i--;
    //@ assert i<100;
    i++;
  }
}
