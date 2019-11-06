/* run.config
   STDOPT: #"-eva-traces-domain -value-msg-key d-traces -slevel 10 -eva-traces-project" +"-then-last -val -print -value-msg-key=-d-traces"
*/

int g;

int main(int c){
  int tmp = 4;
  if(tmp){
    g = tmp;
  } else {
    g = 1;
  }
  return g+1;
}
