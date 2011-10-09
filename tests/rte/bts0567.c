/* run.config 
   OPT: -rte -rte-precond -print -journal-disable
*/

int tab [2] ;

//@ requires \valid(p+1) ;
void f(int *p) ;

void g(){
  f(tab) ;
}
