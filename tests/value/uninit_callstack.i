/* run.config*
   OPT: -no-autoload-plugins -load-module eva -eva @EVA_CONFIG@ -eva-no-show-progress -eva-print-callstacks -journal-disable -no-results
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
