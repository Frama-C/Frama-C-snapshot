/* run.config
OPT: @PTEST_DIR@/@PTEST_NAME@_1.i @PTEST_DIR@/@PTEST_NAME@_2.i -eva @EVA_CONFIG@
OPT: @PTEST_DIR@/@PTEST_NAME@_2.i @PTEST_DIR@/@PTEST_NAME@_1.i -eva @EVA_CONFIG@
*/
extern int a[] ;

/*@ assigns a[3] \from \nothing; */
void g();
