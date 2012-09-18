/* run.config
   STDOPT: +"-load-script tests/syntax/syntactic_hook.ml"
*/

int f(void);

int h(const int*);

int h(int *x) { return *x; } // warns different decls.

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

int f(int); //warns conflicting decls
