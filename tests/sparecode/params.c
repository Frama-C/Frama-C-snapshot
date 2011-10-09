/* run.config
   OPT: -sparecode-debug 1 -sparecode-analysis -journal-disable
   OPT: -slicing-level 2 -slice-return main -journal-disable -then-on 'Slicing export' -print
*/

/* This is an example from #529. 'y' in [main1] should be visible to get a
 * compilable result. But unfortunatly, this leads to also select [b=1] in
 * [main]. This should be enhanced... */

int main1 (int x, int y, int z){
  y = 3;
  return y;
}

int main (void) {
  int a = 0, b = 1, c = 3;
  return main1 (a, b, c);
}

