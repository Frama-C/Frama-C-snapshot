/* run.config
   OPT: -load-script tests/misc/vis_spec.ml
*/

//@ assigns \nothing;
void g () ;

//@ assigns \nothing;
void f () { g(); }

