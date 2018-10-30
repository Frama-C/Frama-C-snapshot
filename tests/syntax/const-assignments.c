/* run.config
   STDOPT:
   STDOPT: +" -cpp-extra-args=-DT0"
   STDOPT: +" -cpp-extra-args=-DT1"
   STDOPT: +" -cpp-extra-args=-DT2"
   STDOPT: +" -cpp-extra-args=-DT3"
   STDOPT: +" -cpp-extra-args=-DT4"
   STDOPT: +" -cpp-extra-args=-DT5"
   STDOPT: +" -cpp-extra-args=-DT6"
   STDOPT: +" -cpp-extra-args=-DT7"
   STDOPT: +" -cpp-extra-args=-DT8"
*/

/* The first run is correct. The others should fail, as they include invalid
   assignments to const lvalues. */
const int x = 1;


#ifdef T0
void f() {
  x = 42;
}
#endif

#ifdef T1
void f() {
  x++;
}
#endif

#ifdef T2
void f() {
  --x;
}
#endif

#ifdef T3
void f() {
  x += 3;
}
#endif

#ifdef T4
void f() {
  const int x = 2;
  x *= 2;
}
#endif

#ifdef T5
void f(const int* x) {
  *x = 1;
}
#endif

extern void g(int *p);

#ifdef T6
void f() {
  g(&x);
}
#endif

#ifdef T7
void f(const int* x) {
  g(x);
}
#endif

void h(const int* x) {
  int* y = (int *)x;
  *y = 1;
  g(y);
}

typedef struct {
 __attribute__((__fc_mutable)) int x;
 const int y;
} S;

void build_S(
  __attribute__((__fc_initialized_object)) const S* s, int x, int y)
{
  s->x = x;
  s->y=y;
}

void mutable_test(const S* s) {
  s->x = 42;
  s->x++;
  s->x += 2;
}

#ifdef T8

typedef struct {
__attribute__((__fc_mutable)) S s;
} T;

void mutable_test_ko(const T* t) {
  t->s.y = 32; // KO: although t->s could be modified, t->s.y is still const
}

#endif
