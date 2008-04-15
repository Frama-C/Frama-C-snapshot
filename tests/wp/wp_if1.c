
//@ ensures ( (!(c==0)) ==> \result == 1) && ((c==0) ==> \result == 2);
int main(int c) {
  int x;
  x = x;
  if (c) x = 1; else x = 2;
  return x;
}
