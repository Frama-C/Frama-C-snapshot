/* run.config
   OPT:  -deps -slice-return main -journal-disable -then-on 'Slicing export' -set-project-as-default -print  -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i  -no-deps
*/
int main() {
  int a,c; volatile int b = 0;
  a = 1;

  while (1) break ;

  for (c=0; c <= 5; c++) ;
  /*@ loop assigns c, a ; */
  for (c=0; c <= 5; c++) { a = 2; }

  if (b) goto L;

  for (c=0; c <= 5; ) {
    a+=2 ;
 L: a+=3;
    goto H;
    c++;
    }

  a++;


 H:
  if (a) c++;
  return a;
}
