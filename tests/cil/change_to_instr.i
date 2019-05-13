/* run.config
OPT: -load-script tests/cil/change_to_instr.ml -print
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
}
