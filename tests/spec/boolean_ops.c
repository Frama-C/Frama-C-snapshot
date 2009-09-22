/*run.config
DONTRUN: boolean operations must be supported at the term level.
*/
/*@ ensures (x==0||y==1)?\result==0:\result == 1; */
int f(int x, int y) { return (x==0||y==1); }

int main() {
  int x = f(42,1);
  int y = f(0,36);
  return 0;
}
