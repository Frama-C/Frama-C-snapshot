/* run.config
   OPT: -load-script tests/misc/vis_spec.i
*/

//@ assigns \nothing;
void g () ;

//@ assigns \nothing;
void f () { g(); }

