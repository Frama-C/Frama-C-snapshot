
/* run.config_phoare
  OPT:  -journal-disable -wp -wp-model Hoare -wp-proof alt-ergo -wp-print -wp-verbose 2
*/

int G;
void main(int a, int b) {
  if (a > b) { int tmp = a; a = b; b = tmp; }
  /*@ assert  a <= b; */
}

void main1( int a, int b, int c, int d) {
  int tmp;

  if (a > b) { tmp = a; a = b; b = tmp; }
  if (c > d) { tmp = c; c = d; d = tmp; }
  if (a > c) { tmp = a; a = c; c = tmp; }
  if (b > d) { tmp = b; b = d; d = tmp; }
  if (b > c) { tmp = b; b = c; c = tmp; }
  /*@ assert a <= b <= c <= d; */
}

