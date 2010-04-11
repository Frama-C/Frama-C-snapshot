/* run.config
   OPT: -keep-switch -print -check -journal-disable
*/

int foo18(int a) {
   int x = 0, y = 1;

   x = ({ 3; goto l ; l: 3;});
   goto l;
   return x;
}

void foo(int z) {
  int i;
  for (i=0; i<10; i++)
    {
    __Cont:
      z++;
      if (z < 5)
        continue;
      else
        goto __Cont;
    }
}

void f() {
  int i = 0;
 while_1_break:
  while (i < 10) {
    ++i;
  }
  goto while_1_break;
}
