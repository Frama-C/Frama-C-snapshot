/* run.config
   DONTRUN: change pretty printing of ghost statements before
*/
//@ assigns *p;
float g(float* p);

void f(float a) { /*@ ghost float x = g(&a); */ }

