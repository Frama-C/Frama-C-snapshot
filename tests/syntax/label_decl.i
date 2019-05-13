/* run.config
MACRO: TMP @PTEST_DIR@/result/@PTEST_NAME@.i
OPT: -print -then -print -ocode @TMP@ -then @TMP@ -print -ocode=""
*/
struct s { int i; };

void s_cp (struct s *p, struct s v) { *p = v; }

void main(void)
{
  int i = 0;
  label: if (i);

  struct s y;

  if ((i < 0) || (i >= 256))
    s_cp(&y, (struct s){1});
}
