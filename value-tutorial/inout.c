void f() {
  int x, y;
  x = 1;
  y = x;
  Frama_C_dump_each();
}

void g(int *p, unsigned int length) {
  for (int i=0; i<length; i++) {
    p[i] = i;
  }

  for (int j=0; j<length; j++) {
    p[length-j-1] = p[j]+j;
  }
  Frama_C_dump_each();
}

void main() {
  int t[10], u[50];

  f();
  
  g(t, 10);
  g(u, 40);
}
