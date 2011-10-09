struct st { int t[10]; } S;
int i,j ;

//@ ensures S.t[i] == s.t[j] + y[x];
void main (struct st s, int x, int *y) {
  S.t[i] = s.t[j] + y[x];
}
