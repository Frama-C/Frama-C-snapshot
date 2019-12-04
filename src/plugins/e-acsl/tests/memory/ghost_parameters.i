/* run.config
   COMMENT: ghost parameters
   STDOPT:
*/

void function(int a, int b) /*@ ghost(int c, int d) */ {

}

int main(void){
  int w = 0 ;
  int x = 1 ;
  //@ ghost int y = 2 ;
  //@ ghost int z = 3 ;

  function(w, x) /*@ ghost(y, z) */;
}
