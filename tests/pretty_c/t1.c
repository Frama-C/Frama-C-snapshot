static int a;
static int b = 4;

int x = 1;
int *p = &x;
short s = -3;
unsigned long long d = 1;

int main (int c) {
  f();
  a--;
  x = !!c;
  p = p + 1;
  int * q = Frama_C_alloc_size(sizeof(int));
  *q = 12;
  Frama_C_dump_each();
  Frama_C_dump_assert_each(); 
  Frama_C_dump_assignments_each(); 
  return b;
}
