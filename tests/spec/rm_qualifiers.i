/* run.config
OPT: @PTEST_FILE@ -ocode @PTEST_DIR@/result/@PTEST_NAME@_res.i -print -then @PTEST_DIR@/result/@PTEST_NAME@_res.i -ocode="" -print
*/
extern void G(const void* p);
typedef volatile int ARR[42][3];
void* ptr;

//@ ensures ptr == (void *)ftab[1];
void F(const ARR ftab, const unsigned id) {
  G((void *)ftab[1]);
}
