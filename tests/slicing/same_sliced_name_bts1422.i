/* run.config
OPT: -val-show-progress -main foo -slice-value y -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check 
*/

int y;

void foo(int x);

void foo(int x) { x++; y++; }

void (*ptr)(int x) = &foo;
