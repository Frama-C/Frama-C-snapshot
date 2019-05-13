/* run.config
   EXECNOW: make -s @PTEST_DIR@/plugin_log.cmxs
   LOG: log-file-kernel-warnings.txt
   LOG: log-file-kernel-results.txt
   LOG: log-file-feedback.txt
   LOG: log-file-value-all.txt
   LOG: log-file-value-default.txt
   LOG: plugin-log-all.txt
   FILTER: sed 's|Your Frama-C version is.*|Your Frama-C version is VERSION|'
   STDOPT: #"-kernel-log w:@PTEST_RESULT@/log-file-kernel-warnings.txt,r:@PTEST_RESULT@/log-file-kernel-results.txt -eva-log f:@PTEST_RESULT@/log-file-feedback.txt,afewr:@PTEST_RESULT@/log-file-value-all.txt -eva-log :@PTEST_RESULT@/log-file-value-default.txt -then -kernel-log f:@PTEST_RESULT@/log-file-feedback.txt"
   OPT: -load-module tests/misc/plugin_log -kernel-msg-key foo-category -kernel-log=a:@PTEST_RESULT@/plugin-log-all.txt
   DONTRUN: test disabled due to non-deterministic errors in CI
 */
int f(void); // generates kernel warning (missing spec)

//@ assigns \result;
int g(void); // generates value warning (missing \from)

int main() {
  f();
  int r = g();
  for (int i = 0; i < 1; i++); // generates value feedback
  return 0;
}
