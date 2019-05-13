/* run.config
   OPT: -eva -load-module scope,eva,report -then -report
   OPT: -wp-prop=@check
   OPT: -wp-prop=@assert
*/
/* run.config_qualif
   OPT: -load-module report -wp-steps 5 -then -report
*/

// note: eva and wp gives the same reporting

//@ axiomatic A { predicate P reads \nothing ; }
void main() {
  //@check  c1: P;
  //@assert a1: P;
  //@check  c2: P;
  //@assert a2: P;
  ;
}
