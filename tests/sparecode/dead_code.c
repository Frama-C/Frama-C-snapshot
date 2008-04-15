/* run.config
   OPT: -sparecode-analysis -journal-disable
   OPT: -slicing-level 2 -slice-return main -slice-print -journal-disable
*/

int main (void) {
  int c = 1, x;
  x = 0;
  if (c)
    x = 1;
  else
    x = 2;
  return x;
}
