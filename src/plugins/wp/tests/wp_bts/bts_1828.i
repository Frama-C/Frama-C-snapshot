/* run.config_qualif
   OPT:
   OPT: -wp-model "+ref"
 */

//@ requires \valid(one);
void f(int *one){
  int two;
  //@ assert ok: one != &two;
}

int *zero;
//@ requires \valid(zero) && \valid(one);
void frame(int *one, int arg){
  int two;
  //@ assert ok: formal: \separated(one,&arg,&two);
  //@ assert ok: global: \separated(zero,&arg,&two);

  // zero and one may overlaps, so next property isn't true.
  //@ assert ko: \separated(zero,one);
}
