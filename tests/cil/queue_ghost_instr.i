/* run.config
OPT: -load-script tests/cil/queue_ghost_instr.ml -print
*/


int main(){
  int i = 0 ;
  //@ ghost int j = 0 ;

  i++ ;
  //@ ghost j++ ;

  {
    //@ ghost int x = 0;
    //@ ghost x++ ;
  }

  if(i){
    ;
  }

  /*@ ghost if(j){

    }
  */
}
