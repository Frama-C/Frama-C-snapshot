/* run.config
OPT: -journal-disable -print
OPT: -journal-disable -semantic-const-folding
OPT: -journal-disable -sparecode-analysis
*/

int main1 (void) { 
  int r ;
  if (1) r = 0; else r = 2;
  return r;
}

int main2 (void){ 
  int r = 1;
  if (r) r = 0; else r = 2;
  return r;
}
int main (void) {
  int x1 = main1();
  int x2 = main2();
  return x1 + x2;
}
