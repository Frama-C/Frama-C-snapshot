/* run.config
STDOPT: +"-orig-name"
*/

int x = 1;

int f(int x) {
  int y = 0;
  if (x == 0) {
    int x = 3;
    y = x++;
  }
  y += x;
  return y;
}
