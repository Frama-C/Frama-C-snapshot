/* run.config
   GCC:
   OPT: -val -deps -out -input -inout -journal-disable
*/

char t[100]={0,1,2,3,4,5,6,7,8,9};
char *p;
int x;

char t1[100]={0,1,2,3,4,5,6,7,8,9};
char *p1;
int x1;

void main(void)
{
  int mask = 7;

  p = (char*)(((int)(t + 7)) & ~7);
  *p = 5;
  x = *p;

  p1 = (char*)(((int)(t1 + mask)) & ~mask);
  *p1 = 5;
  x1 = *p1;
}
