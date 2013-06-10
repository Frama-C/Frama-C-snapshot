/* run.config
   STDOPT: +" -then -lib-entry"
 */
struct s;

//@ assigns *p \from \nothing;
void f(struct s *p);

void invalid_assigns_imprecise() {
  struct s *p = 0;
  f(p); // p is invalid, but could be considered valid since sizeof(*p) = Top
}

void write_garbled() { // Write through a garbled mix
  int i = 1;
  int j = 2;
  int k[5] = { 2, 3};

  int *p = &j + (int) &k;
  *p = 1;
  Frama_C_dump_each();
  *p = p;
}

volatile int v;

struct s v1, v2;
struct u v3, v5;
struct s t[10];
// struct ss { struct s f1; int f2; }; Does not parse

void Frama_C_memset(unsigned char*p, int c, unsigned long);

void abstract_structs() {
  char *p = &v1;
  if (v) {
    char w1 = *p+1;
  }
  if (v) {
    char w = *p;
  }
  if (v) {
    struct s v4 = v1;
  }
  *p = 1;
  char q = *p;
  if (v) {
    v1 = v2;
  }
  v2 = v1;
  Frama_C_memset(&v3, -5, sizeof(v3));
  int *p2 = ((int*)&v2)+1;
  *p2 = &v;
  t[5] = v2;
  char *p4 = ((char*)&v5) + (short)v;
  *p4 = 18;
  char *p4 = ((char*)&v5) + (signed int)v;
  *p4 = 19;
  char *p4 = ((char*)&v5) + (unsigned int)v;
  *p4 = 20;
}

void cast_address() {
  int x;
  int *p = &x;
  char c1 = p;
  char c2 = *((char*)&p);
  char c3 = *((char*)&p)+0;
}

void main() {
  invalid_assigns_imprecise();
  write_garbled();
  abstract_structs();
  cast_address();
}
