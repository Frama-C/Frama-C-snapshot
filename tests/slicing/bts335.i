/* run.config
   OPT: -val-show-progress -slice-pragma g -calldeps -slicing-level 3 -journal-disable -then-on 'Slicing export' -set-project-as-default -print -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -no-calldeps
*/

/*
bin/toplevel.opt -slice-pragma g -calldeps -slicing-level 3 tests/slicing/bts335.c -debug 2
bin/toplevel.opt -pdg-debug -pdg -pdg-debug "-pdg-pot bts335" tests/slicing/bts335.c 
 */
int T[2]  = {0, 0};
void f (int i) { T[i]++; }
void g (void) { f(0); /*@ slice pragma expr T[0]; */ }
void main (int c) { if (c) g(); else f(1); }
