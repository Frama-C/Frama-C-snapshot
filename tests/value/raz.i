volatile int h;

int main() {
  int n = h?0:10;
  int r = 0, i;
  // @ ensures i==n
  // @ invariant 0 <= i && i <= n
  for (i=0; i<n ; i++)
    r = 1;
  return r;
}
/*
void main0() {
  int n = 10;
  int r = 1;
  //@ ensures r == 0
  if (r) r = 0; 
  //@ ensures r == 0
  for (int i=11; i<5 ; i++)
    r = 0;
}
*/
