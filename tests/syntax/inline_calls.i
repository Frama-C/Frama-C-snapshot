/* run.config
   STDOPT: +"-inline-calls @all"

 */

int f() {
  return 2;
}

inline int in_f() {
  return 3;
}

volatile int v;
int g() {
  if (v) return f();
  else return in_f();
}

int h() {
  return g();
}

int i() {
  /*@ assert i:\true; */
  return 0;
}

int rec(int x) {
  if (x < 0) return x;
  return rec(x-1);
}

int f1(int);
int g1(int);
volatile int nondet;

int f1(int a) {
  if (nondet)
    g1(1);
  else if (nondet) f1(2);
}

int g1(int a) {
  if (nondet)
   g1(4);
  return a;
}

int main() {
  int local_init = i();
  int t = rec(local_init);
  f1(2);
  return h();
}


int with_static() {
  static int count = 0;
  count++;
  return count;
}

int call_with_static () {
  return with_static();
}

void builtin_acsl() {
  float g = 0.f;
  /*@ assert ¬\is_NaN(g); */
}

void call_builtin_acsl () {
  builtin_acsl();
}

void f_slevel() {
  //@ slevel 0;
  return;
}

void call_f_slevel () {
  f_slevel();
}

void pre_decl(void);

void middle_decl() {
  pre_decl();
}

void post_decl(void);

extern int x;

void pre_decl() {
  extern int y = 23;
  x++; y++;
  post_decl();
}
