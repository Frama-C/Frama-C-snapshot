/* run.config
   DONTRUN: don't run a test which raises an exception
   OPT: -sparecode-debug 1 -sparecode-analysis -journal-disable -main main_top
   OPT: -sparecode-debug 1 -sparecode-analysis -journal-disable -main main_call_top
*/

int f (int a) {
  return a+1;
}

int main_top (int nb, ...) {
  int x = 3;
  int y = f (2);
  return x;
}

int main_call_top (void) {
  int x = main_top (2, 0, 1);
  return x;
}
