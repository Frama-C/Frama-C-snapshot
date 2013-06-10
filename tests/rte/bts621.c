/* run.config
   OPT: -print -then -no-print -rte -warn-signed-overflow -rte-precond -then -print
*/
//@ assigns *p;
float g(float* p);

void f(float a) { /*@ ghost float x = g(&a); */ }

