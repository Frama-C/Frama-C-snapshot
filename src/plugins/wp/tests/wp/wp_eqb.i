
/* run.config
   OPT: -wp-msg-key print-generated -wp-prover alt-ergo -wp-gen
*/


/*@
  ensures \result == 1 ==> ((a==b) == (c==d));
 @*/
int f(int a, int b, int c, int d){
  if((a==b) == (c == d)){
    return 1;
  } else {
    return 0;
  }
}
