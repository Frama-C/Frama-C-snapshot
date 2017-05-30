/* run.config
   OPT: -no-autoload-plugins -load-module report -load-script tests/report/projectified_status.ml
   OPT: -no-autoload-plugins -load-module report -load-script tests/report/no_hyp.ml
   OPT: -no-autoload-plugins -load-module report -load-script tests/report/multi_emitters.ml
*/

void main() {
  int x = 1;
  /*@ assert \true; */
}
