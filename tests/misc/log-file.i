/* run.config
   STDOPT: #"-kernel-log w:@PTEST_RESULT@/log-file-kernel-warnings.txt,r:@PTEST_RESULT@/log-file-kernel-results.txt -value-log f:@PTEST_RESULT@/log-file-feedback.txt,afewr:@PTEST_RESULT@/log-file-value-all.txt -value-log :@PTEST_RESULT@/log-file-value-default.txt -then -kernel-log f:@PTEST_RESULT@/log-file-feedback.txt"
   LOG: log-file-kernel-warnings.txt
   LOG: log-file-kernel-results.txt
   LOG: log-file-feedback.txt
   LOG: log-file-value-all.txt
   LOG: log-file-value-default.txt
   LOG: plugin-log-all.txt
   OPT: -load-module tests/misc/plugin_log.ml -kernel-msg-key foo-category -kernel-log=a:@PTEST_RESULT@/plugin-log-all.txt
 */
volatile int a = 42; // generates value warning

int f(); // generates kernel warning

int main() {
  int i;
  f();
  for (i = 0; i < 1; i++); // generates value feedback
  return 0;
}
