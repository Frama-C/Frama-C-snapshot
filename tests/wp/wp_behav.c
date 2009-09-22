/* run.config
OPT: -wp-mm 0 -wp-behav x1 -wp-debug 1 -journal-disable  -wp-no-proof
OPT: -wp-mm 0              -wp-debug 1 -journal-disable  -wp-no-proof
*/
/* run.config_dev
OPT: -wp-mm 0 -wp-behav x1 -wp-debug 2 -journal-disable  -wp-proof
OPT: -wp-mm 0              -wp-debug 2 -journal-disable  -wp-proof
*/

/*@
  @ ensures \result > x;
  @ behavior x1:
  @   assumes x == 1;
  @   ensures \result == 3;
  @ behavior x2:
  @   assumes x == 2;
  @   ensures \result == 4;
  @
*/
int f (int x) {
  x++;
  //@ for x1: assert x == 2;
  //@ for x2: assert x == 3;
  return x+1;
}

