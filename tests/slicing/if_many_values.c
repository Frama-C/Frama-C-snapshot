/* run.config
   OPT: -check -slice-value r -journal-disable -slevel 101 -then-on 'Slicing export' -print
 **/

int r=1;

int main() {
  for (int i = -100; i < 100; i++) {
    if (i != 0)
      if (i)
        r += 1;
  }
  return r;
}
