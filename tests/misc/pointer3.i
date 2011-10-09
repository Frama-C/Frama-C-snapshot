int x,y,c;
int *p,*q;

int* f(int * x) {
  c=2;
  return x;
}

void main() {
  c=1;
  p = f(&x);
  q = f(&y);
  *p = c;

}
