/* run.config 
OPT: -load-module lib/plugins/Report  -pp-annot -val -then -report
*/

/*@ requires \valid(&t[0..s-1]);
    requires 1 <= c < s; */
void init (int *t, int c, int s) {
  int* p = t;
  /*@ loop invariant \valid(p) && p < &t[s-1]; */
  while(1) {
    *(++p) = 1;
    if(p >= t+c) break;
  }
}


void main (int c) {
  int t1[72];
  int t2[11];

  if (c >= 1 && c < 72) {
    init(t1, c, 72);

    if (c < 8)
      init(t2, c, 11);
  }
}
