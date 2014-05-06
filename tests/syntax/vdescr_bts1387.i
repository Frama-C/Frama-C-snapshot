/* run.config
OPT: -load-script tests/syntax/vdescr_bts1387.i 
*/
int f(int);
int g(int);

int (*fptr)(int, int);

int main(int j, int k)
{
  int a, b, c, d, *p, t[2];
  a = 1 + j++;
  p = j ? &j : &k;
  a = (*p) + 1;
  a = (*p) + 1;
  a = (*p++) + 1;
  p--;
  a = (*p++) + 1;
  p--;
  b = ++j + 1;
  c = f(j) + g(j);

  d = 3 + *(k?&j:(int*)0);

  d = 100 / j;

  fptr = f;
  if (d & 1) (*fptr)(1, 2);

  if (d & 2) t[0] = t[1] + 1;

  t[d & 4] = j;
  j = t[d & 8];

  {
    int l;
    p = &l;
  }

  if (d & 16) *p = 1;

  return j;
}
