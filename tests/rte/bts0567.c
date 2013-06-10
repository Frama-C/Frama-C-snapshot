/* run.config 
   OPT: -rte -warn-signed-overflow -rte-precond -print
*/

int tab [2] ;

//@ requires \valid(p+1) ;
void f(int *p) ;

void g(){
  f(tab) ;
}
