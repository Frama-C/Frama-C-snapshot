/* run.config
   DONTRUN: bts 0727. Not fixed yet
   OPT: -load-script tests/misc/vis_spec.i
*/

//@ assigns \nothing;
void g () ;

//@ assigns \nothing;
void f () { g(); }

