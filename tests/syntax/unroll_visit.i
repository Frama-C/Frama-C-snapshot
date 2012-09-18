/* run.config
   STDOPT: +"-val" +"-deps" +"-out" +"-input" +"-deps"
 */
void main() {
   /*@ loop pragma UNROLL 2; */
  for(int i=0; i<100; i++) {
    i--;
    //@ assert i<100;
    i++;
  }
}
