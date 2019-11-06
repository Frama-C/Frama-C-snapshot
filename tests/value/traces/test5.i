/* run.config
   STDOPT: #"-eva-traces-domain -value-msg-key d-traces -slevel 10" +"-then-last -val -slevel 10 -print -no-eva-traces-domain"
*/


/* Check the fix for the creation of expression by dataflows2 for
   switch (conversion to list of if) */

int myswitch(i){
  switch(i){
  case 0: return 0;
  case 1: return 1;
  default: return 2;
  }
}

int main(c){
  int tmp = 1;
  for(int i = 0; i < 10; i++){
    for(int j = 0; j < 10; j++){
      tmp = my_switch(tmp) + tmp;
    }
  }
  return tmp;
}
