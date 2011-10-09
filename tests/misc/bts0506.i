/*run.config
STDOPT: +"-no-collapse-call-cast" +"-print"
STDOPT: +"-collapse-call-cast" +"-print"
*/

int f(int x) { return x+1; }

int main () {
  short x = 4;
  x = f(42);
  return 0;
}
