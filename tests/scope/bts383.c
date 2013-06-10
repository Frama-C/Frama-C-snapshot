/* run.config
   OPT: -val -print -journal-disable -scope-verbose 1 -remove-redundant-alarms -context-width 3
*/
/* 
   echo '!Db.Scope.check_asserts();;' \
   | bin/toplevel.top -val tests/scope/bts383.c 
*/
int v;
void if1 (int * p) {
  if (*p > 0) 
    v = *p;
}
int if2 (int c, int * p) {
  if (c)
    v = *p;
  return *p;
}
void loop1 (int * p) {
  int i;
  int n = *p;
  for (i = 0; i < n; i++) {
    v = *p;
  }
}
int loop2 (int n, int * p) {
  int i;
  for (i = 0; i < n; i++) {
    v = *p;
  }
  return *p;
}
void out_char (char c);
void out_string (const char *value)
{
  for(; *value; value++)
    out_char(*value);
}
typedef struct { int a; int b; } Tstruct;
int fstruct (Tstruct * ps) {
  int x;
  ps->a = 3;
  ps->b = 5;
  ps->a = ps->b;
  ps->b = ps->a;
  x = ps->a + ps->b;
  ps++;
  ps->a = 3;
  ps->b = 5;
  ps->a = ps->b;
  ps->b = ps->a;
  x += ps->a + ps->b;
  return x;
}
int main (int * p, Tstruct * ps) {
  int x;
  x = *(p+1);
  v = *(p+1);
  if1(p+1);
  if2(x,p+1);
  loop1(p+1);
  loop2(x,p+1);
  out_string(p+1);
  x += fstruct (ps+1);
  return x;
}
