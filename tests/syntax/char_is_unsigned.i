/* run.config 
   OPT:-print -load-script tests/syntax/machdep_char_unsigned.ml -machdep unsigned_char -then -constfold -rte -rte-all
*/
char t[10];

void main() {
  int r = (t[0] == 'a');
  char c = 455;
}
