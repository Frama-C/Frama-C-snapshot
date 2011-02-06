int a = -12;
int b;
unsigned int *p=&a;
unsigned int *q=&b;
int X, Y, Z, T;

main(int c){
  b = c ? -1 : 5;
  if (*p == 3) 
    X = *p;
  else
    Y = *p;
  if (*q == ((unsigned int)-1))
    Z = *q;
  else
    T = *q;
}
