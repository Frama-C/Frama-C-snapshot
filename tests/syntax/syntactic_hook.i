/* run.config
   EXECNOW: make -s @PTEST_DIR@/@PTEST_NAME@.cmxs
   STDOPT: +"-no-autoload-plugins -load-module @PTEST_DIR@/@PTEST_NAME@.cmxs"
*/
int f(void);

int k(int *);

int k(int * x) { return (*x)++; }

int main () {
  int x = 0; int y = 0;
  int t(void);
  x=t();
  x++;
  x; // warn ignore pure exp
  g(3); // warn implicit proto
  x = sizeof(x++); // warn drop side-effect
  x = x++ && x;
  y = x && x++; // warn conditional side-effect
  y = x && (x++ || x); // warn conditional side-effect
  y = x && (x || x++); // warn conditional side-effect
  y = x ? x++ : x++; // warn conditional side-effect
  return x;
}

int f(int); //error: conflicting decls
