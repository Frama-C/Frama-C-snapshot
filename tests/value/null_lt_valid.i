int t[23];
int *p, *q, *r;

void f(void){
  if (p < t)
    *p = 1;
}

void g(void){
  int *q1, *q2;
  if (q < t+22)
    q1 = q;
  else
    q2 = q;
}

void h(void){
  int *r1, *r2;
  if (r < t+22)
    r1 = r;
  else
    r2 = r;
}

int main(int c){
  if (c&32)
    f();
  q = (c&64) ? t+(c&15) : p;
  if (c&128)
    g();
  r = (c&256) ? t+(c&31) : p;
  if (c&512)
    h();
  t[0] = (p < t);
  t[1] = (q < t + 22);
  t[2] = (r < t + 22);
  return 0;
}
