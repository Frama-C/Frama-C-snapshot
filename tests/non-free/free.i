void Frama_C_free(void*);
void* Frama_C_alloc_size(unsigned long);

void main(volatile int foo) {
  int *p = Frama_C_alloc_size(40);
  p[1] = 1;
  int *q = Frama_C_alloc_size(40);
  q[2] = 2;
  int *r = foo ? p : q;
  Frama_C_dump_each();
  Frama_C_free(r);

  int *u = Frama_C_alloc_size(40);
  u[3] = 3;
  Frama_C_free(u);

  int* r = 0;
  Frama_C_free(r);

  int* s = Frama_C_alloc_size(40);
  s[4] = 4;
  s = foo ? 0 : s;
  Frama_C_free(s);
}
