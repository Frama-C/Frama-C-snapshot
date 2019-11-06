/* run.config
   STDOPT: #"-eva-traces-domain -value-msg-key d-traces -slevel 10"
*/

/* Test of join inside a loop */

int main(c){
  int tmp = 0;
  for(int i = 0; i < 100; i++){
    if(i % 2){
      tmp ++;
    };
    if(i % 3){
      tmp ++;
    };
    if(i % 5){
      tmp ++;
    };
    if(i % 7){
      tmp ++;
    };
    if(i % 11){
      tmp ++;
    };
    tmp++;
  }
  return tmp;
}
