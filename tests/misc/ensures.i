/* run.config
   OPT: -load-script tests/misc/ensures.ml
*/
//@ ensures *p==1;
void main(int * p){ *p = 0; }
