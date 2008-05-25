int x,y,c,*p,*q,T[10];

void g() {
  p = (int*)(void*)&y;
  *p = c;
}

void f1() {
  x = y;
  q= &x;
  if (c) p = &x;
  p = &c;
// p = &T[c];
  *p = *q;
}

/*@ ensures x > 0; */
void h() {
  p = &x;
  c = *p;
  *p = y;
}


void l(int *y) {
  *y = x;
}
void k(int *x) {
  l(x);
}

void main() {
  c=17;
  x=19;
  k(&c);
  k(&x);
}
