/* run.config
   OPT: -rte -warn-signed-overflow -rte-precond -print
*/

typedef int (*fptr)(int);

void g() { return; }

int f(int x) { return x; }
int h(int x) { return x; }

int main (int i)
{
  void (*fp1)();
  fptr fp2;
  fptr ma[2] = { &f, &h };
  
  fp1 = &g;
  fp2 = &f;

  (*fp1)();
  (*fp2)(3);
  (*ma[1])(5);
  (*ma[i])(5);
  return 0;  
}
