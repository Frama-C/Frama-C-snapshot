/* run.config
   OPT: -load-script tests/report/projectified_status.ml
   OPT: -load-script tests/report/no_hyp.ml
   OPT: -load-script tests/report/multi_emitters.ml
*/

void main() {
  int x = 1;
  /*@ assert \true; */
}
