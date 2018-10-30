/* run.config*
   STDOPT: #"-val-malloc-functions alloc,Frama_C_malloc_by_stack -val-mlevel 0"
*/

#define N 2000

int t[N];

void f() {
  for (int i=0; i<N; i++)
    t[i] = i;
}

int *alloc() {
  return Frama_C_malloc_by_stack(4);
}

int *k() {
  return alloc();
}

void main() {
  f();
  f();
  f();
  Frama_C_show_each(t[1]);
  Frama_C_show_each(t[1]);
  Frama_C_show_each(t[2]);
  f();

  int *p1 = alloc();
  int *p2 = alloc();

  int *p3 = k();
  int *p4 = k();

}
