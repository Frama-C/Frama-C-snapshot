/* run.config
OPT: @PTEST_DIR@/@PTEST_NAME@_aux.i -print
*/
int open (const char* file, int flags, int mode) {
  return -1;
}

/*@ assigns \result \from x; */
int foo (int x, int y);
