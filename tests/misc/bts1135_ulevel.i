/* run.config
   OPT: -ulevel 2 -typecheck -print
 */

/* small test cases to verify C labels are correclty managed into annotations */
int X ;
void main (int c) {
  for (int i = 0 ; i < 10 ;) {
    if (c) 
      //@ ensures \false ;
      goto there ;
    X++;
  there: i++;
    //@ assert c==0 ==> \at(X,there)==i+1;
  }
}
		
