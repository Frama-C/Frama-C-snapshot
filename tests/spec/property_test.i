/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   OPT: -no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs
*/

int X;

/*@ requires X >= 0;
    ensures X >= 0;
*/
int main (int c) {
  if (c) X++;
  /*@ assert X >= \at(X,Pre); */
  return X;
}
