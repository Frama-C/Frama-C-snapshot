/* run.config
   OPT: -sparecode-debug 1 -sparecode -val-show-progress -journal-disable
   OPT: -slicing-level 2 -slice-return main -val-show-progress -journal-disable -then-on 'Slicing export' -print
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
