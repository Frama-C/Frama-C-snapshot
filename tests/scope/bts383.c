/* run.config
   OPT: -val -print -journal-disable -scope-verbose 1 -scope-debug 1
*/
/* 
   echo '!Db.Scope.check_asserts();;' \
   | bin/toplevel.top -val tests/scope/bts383.c 
*/

void if1 (int * p) {
  if (*p > 0) 
    (*p)++;
}
int if2 (int c, int * p) {
  if (c)
    (*p)++;
  return *p;
}
void loop1 (int * p) {
  int i;
  int n = *p;
  for (i = 0; i < n; i++) {
    (*p)++;
  }
}
int loop2 (int n, int * p) {
  int i;
  for (i = 0; i < n; i++) {
    (*p)++;
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
  x = ps->a + ps->b;
  ps++;
  ps->a = 3;
  ps->b = 5;
  x += ps->a + ps->b;
  return x;
}
int main (int * p, Tstruct * ps) {
  int x;
  x = *p;
  *p = 3;
  if1(p);
  if2(x,p);
  loop1(p);
  loop2(x,p);
  out_string(p);
  x += fstruct (ps);
  return x;
}
