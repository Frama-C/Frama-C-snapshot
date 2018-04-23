/* run.config_qualif
   OPT: -load-script tests/wp/stmtcompiler_test_rela.ml -wp-msg-key no-time-info -wp-msg-key no-step-info
*/

int empty (int c){
  c = c < 0 ? c + 10 : c+100;
  int tmp;
  tmp = c;
  /*@ assert \true;*/
  return tmp;
}
