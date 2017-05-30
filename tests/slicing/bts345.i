/* run.config
   OPT: -val-show-progress -check -slice-return call_top -main call_top -journal-disable -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check 
   OPT: -val-show-progress -check -slice-return top      -main top -journal-disable -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check 
   OPT: -val-show-progress -check -slice-return top      -main call_top -journal-disable -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check 
   OPT: -val-show-progress -check -slice-return called_by_top -main top -journal-disable -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check 
   OPT: -val-show-progress -check -slice-return called_by_top -main call_top -journal-disable -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check 
*/

int called_indirectly_by_top (int x) {
  x++ ;
  return x ;
}

int called_by_top (int x) {
  x++ ;
  int z = called_indirectly_by_top (x) ;
  return z ;
}

int top (int x, ...) {
  x++ ;
  int z = called_by_top (x) ;
  return z;
}

int call_top (int y) {
  y++;
  int z = top (y) ;
  return z ;
}
