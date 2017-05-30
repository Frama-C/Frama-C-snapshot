/*  run.config
OPT: -val-show-progress -check -slice-rd y -then-on 'Slicing export' -set-project-as-default -print -check -then -print -ocode @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -then @PTEST_DIR@/result/ocode_@PTEST_NUMBER@_@PTEST_NAME@.i -check 
*/
int z1(void);

int x(int y, int z){
/*@ slice pragma expr y == 1; */
//@ assert y == 1;
//@ assert y + z == 3;
 return 2*y*z1();
}

int main()
{
 x(1,2);
 return 0;
}

int z1()
{
 return 1;
}
