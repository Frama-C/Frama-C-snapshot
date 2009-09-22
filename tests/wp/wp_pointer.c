/* run.config
OPT: -wp-mm 1 -wp-debug 1 -journal-disable  -wp-no-proof
OPT: -wp-mm 0 -wp-fct main -wp-debug 1 -journal-disable  -wp-no-proof
OPT: -wp-mm 2 -wp-debug 1 -journal-disable  -wp-no-proof
OPT: -wp-mm 0 -wp-fct f -wp-behav b2 -wp-debug 1 -wp-bot -journal-disable  -wp-no-proof
*/
/* run.config_dev
OPT: -wp-mm 1 -wp-debug 2 -journal-disable  -wp-proof
OPT: -wp-mm 0 -wp-fct main -wp-debug 2 -journal-disable  -wp-proof
OPT: -wp-mm 2 -wp-debug 2 -journal-disable  -wp-proof
OPT: -wp-mm 0 -wp-fct f -wp-behav b2 -wp-debug 2 -wp-bot -journal-disable  -wp-proof
*/

/*@ requires \valid(p+1) ;
  ensures *(p+1) == 56 ; */
int main(int *p) {
  int x = 55;
  *(p+1) = x + 1 ;
  //@ assert 56 == *(p+1) ;
  x++;
  return 0;
}

int G;
/*@ behavior b1 :
  @   ensures \old(G) >=0 ==> G == \old(G);
  @ behavior b2 : // this one seems similar to b1
  @   assumes G >= 0;
  @   ensures  G == \old(G);
*/
void f () {
  int * p;
  p = &G;
  if (G < 0)
    *p = 0;
}
