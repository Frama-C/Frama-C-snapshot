#include<stdbool.h>
bool x;
int y;

int main() {
  x=false;
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
