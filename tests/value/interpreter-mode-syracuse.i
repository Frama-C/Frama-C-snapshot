/* run.config*
   STDOPT: +" -eva-interpreter-mode"
*/

int main()
{
  int x = 127;
  int n = 1;

  Frama_C_show_each(n, x);

  while (x != 1) {
    if (x % 2 == 0)
      x = x/2;
    else
      x = 3*x + 1;
    n++;
    Frama_C_show_each(n, x);
  }

  return n;
}
