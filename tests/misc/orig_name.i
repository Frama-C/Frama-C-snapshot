/* run.config
   CMD: bin/toplevel.byte
   OPT: -orig-name -journal-disable -print
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
