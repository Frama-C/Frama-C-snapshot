/* run.config_qualif
   OPT:
   OPT: -wp-model "+ref" -wp-print-separation
 */

//@ requires \valid(one);
void f(int *one){
  int two;
  //@ assert ok: one != &two;
}

int *zero;

/*@ 
  requires \valid(zero) && \valid(one);
  // zero and one may overlaps: it is only valid with ref model
  ensures ok_iff_ref: \separated(zero,one);
*/
void frame(int *one, int arg){
  int two;
  //@ assert ok: formal: \separated(one,&arg,&two);
  //@ assert ok: global: \separated(zero,&arg,&two);
}
