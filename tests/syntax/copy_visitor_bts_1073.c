/* run.config
OPT: -load-script tests/syntax/copy_visitor_bts_1073.ml
OPT: -load-script tests/syntax/copy_visitor_bts_1073_bis.ml -test -then-on filtered -print
*/

int f(int x);

int f(int x) { return x; }

int g(int y) { return f(2*y); }

//@ assigns \result \from \nothing;
int printf(const char*, ...);

int main (int argc, char * argv[]) {
  int i;

  printf ("Hello !\n");

  for (i = 0; i < argc; i++)
    printf ("arg %d : %s\n", i, argv[i]);

  printf ("Found %d arguments\n", i - 1);

  return 0;
}
