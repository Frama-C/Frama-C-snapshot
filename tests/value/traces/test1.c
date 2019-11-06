/* run.config
   STDOPT: #"-eva-traces-domain -value-msg-key d-traces -slevel 10 -eva-traces-project" +"-then-last -val -print -value-msg-key=-d-traces"
*/

extern volatile int entropy_source;

/*@ requires min <= max;
    assigns \result \from min, max, entropy_source;
    assigns entropy_source \from entropy_source;
    ensures min <= \result <= max ;
 */
extern int interval(int min, int max);


int g = 42;

int main(int c){
  /* c = interval(0,1); */
  int tmp;
  tmp = 0;
  if (c) tmp = g;
  else tmp = 2;
  for(int i = 0; i < 3; i++){
    tmp ++;
  }
  g = tmp;
  return tmp;
}
