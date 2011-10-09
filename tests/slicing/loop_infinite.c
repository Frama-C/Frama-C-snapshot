/* run.config
   OPT: -check -deps -slice-return main -journal-disable -then-on 'Slicing export' -print
*/
int main() {
  volatile int a=0,b,c;
  if (a)
    {a = 1;

  while (1) {
    a++;
    };
  return 0;}
}
