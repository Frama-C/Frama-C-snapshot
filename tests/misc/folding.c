int f(int x) {return x;};

int g() {
  int (*pfct)(int) = &f;
  int un = 1;
  int *p =&un;
  int deux = 1+un;
  int trois ;
  p = &deux;
  trois = *p+*p+un;
  return (*pfct)(trois);
}

int foo(int x, int y) {
    volatile int unknown=0;
    if (unknown)
      return y+2;     
    return x+3;      
  }

int main () {
  int a,b,c;
  g();
  a = foo(5,7) + foo(6,777);
  b = 4;
  c = b * b +a;
  if (b > c)
    return b-c;
  else
    return b+c;
}
