/* run.config
  STDOPT: +"-metrics-ast cabs"
 */

void main() {
  int j = 1;
  //@ loop pragma UNROLL 6;
  for (int i=0; i<6; i++) {
    j += 2;
  }
}
