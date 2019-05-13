/* run.config
   STDOPT: +"-sparecode"
   STDOPT: +"-slicing-level 2 -slice-return main -then-on 'Slicing export' -print"
*/

int main (void) {
  int c = 1, x;
  x = 0;
  if (c)
    x = 1;
  else
    x = 2;
  return x;
}
