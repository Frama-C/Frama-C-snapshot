/* run.config
   OPT: -val -val-show-progress -slevel-function main:10 -load-script tests/misc/issue109.ml
*/

void main() {
  int i, j = 0;
  for (i=0; i<10; i++) {
    j++;
  }
  //@ assert i == j;
}
