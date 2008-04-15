/* run.config
   OPT: -deps -slice-print -slice-return main -journal-disable
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
