/* run.config
   STDOPT: +"-print"
*/

void printf(const char*,...);

_Bool x;
int y;

int main() {
  x=0;
  printf("%d\n",x);
  x=2;
  printf("%d\n",x);
  y=x+1;
  printf("%d,%d\n",x,y);
  x=x+1;
  printf("%d\n",x);
  x=x+1;
  printf("%d\n",x);
  return y;
}
