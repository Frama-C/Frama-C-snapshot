/* run.config
MACRO: TMP @PTEST_DIR@/result/@PTEST_NAME@.i
OPT: -print -then -print -ocode @TMP@ -then @TMP@ -print -ocode=""
*/
void main(void)
{
  int i = 0;
  label: if (i);
}
