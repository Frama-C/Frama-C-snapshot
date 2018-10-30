/* run.config
   OPT: -metrics-used-files @PTEST_DIR@/@PTEST_NAME@1.i @PTEST_DIR@/@PTEST_NAME@2.i @PTEST_DIR@/@PTEST_NAME@3.i @PTEST_DIR@/@PTEST_NAME@4.i @PTEST_DIR@/@PTEST_NAME@5.i @PTEST_DIR@/@PTEST_NAME@6.i @PTEST_DIR@/@PTEST_NAME@7.i @PTEST_DIR@/@PTEST_NAME@8.i @PTEST_DIR@/@PTEST_NAME@9.c @PTEST_DIR@/@PTEST_NAME@1.h @PTEST_DIR@/@PTEST_NAME@2.h
   OPT: -metrics-used-files -main g @PTEST_DIR@/@PTEST_NAME@1.i @PTEST_DIR@/@PTEST_NAME@2.i @PTEST_DIR@/@PTEST_NAME@3.i @PTEST_DIR@/@PTEST_NAME@4.i @PTEST_DIR@/@PTEST_NAME@5.i @PTEST_DIR@/@PTEST_NAME@6.i @PTEST_DIR@/@PTEST_NAME@7.i @PTEST_DIR@/@PTEST_NAME@8.i @PTEST_DIR@/@PTEST_NAME@9.c @PTEST_DIR@/@PTEST_NAME@1.h @PTEST_DIR@/@PTEST_NAME@2.h
*/

int h(void);

int glob;

void indirect(void);

void indirect_unused(void);

int k(void);

int main() {
  void (*fp)() = indirect;
  fp();
  return h() + glob + k();
}
