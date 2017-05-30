/* run.config*
   OPT: -no-autoload-plugins -load-module value -val @VALUECONFIG@ -no-val-show-progress -val-print-callstacks -journal-disable -no-results
*/
int *p, x;

void f(void)
{
  if (*p) x = 1;
}

int main(){
  int a;
  p = &a;
  f();
}
