/* run.config*
   OPT: -no-autoload-plugins -load-module from,inout,value -val @VALUECONFIG@ -val-initialization-padding-globals maybe
*/

int volatile G = 1;
volatile int F, E, X, Y, *pV;

int k = 1, x = 2, y = 3;
int a,b,c,d,e,f,g,h,i,j,l,m,n,o, *pv;

struct s { int a; volatile int b; } s1,s2={1,1};

struct sv { int a; volatile int b; };

volatile struct sv sv1, sv2={1,1};

struct sv sv3 = {3};
struct sv sv4 = {4, 5};

int fn2(int, int);

int fn1(int x, int y)
{
  Frama_C_show_each_1(x);
  Frama_C_show_each_2(y);
  return x + y;
}

struct { struct ss1 { int a; volatile int b;};
         volatile struct ss2 { short c; volatile int d;};
         volatile char t[12];
         short e;}
  nested = { {1}, 0};

int R1, R2;

int main1 () {
  /* passing volatile things to functions */
  R1 = fn1(G, G|0);
  R2 = fn2(G, G|0);
  Frama_C_show_each_d(G);

  G = G;
  k = G;

  /* reading an uninitialized volatile variable */
  a = F ? 11 : 12;

  /* relations involving volatile variables */
  b = F;
  c = F;
  d = b - c; 
  e = F - F;
  g = F;
  f = F - g;
  l = F + 1;
  m = 2 + F;
  n = F - l;
  o = m - l;

  /* lval to lval assignment to volatile variable */
  h = 1;
  E = h;

  /* assignment via pointer */
  X = -1;
  Y = -1; 
  pv = (int *) &X;
  *pv = x; /* assignment to volatile X */
  x = *pv;  
  pV = &Y;
  *pV = y; /* assignment to volatile Y */
  y = *pV;

  return Y;
}

// Test volatile pointers
int * volatile main2() {
  int * volatile p1, * volatile p2, * volatile p3;
  p1 = G ? 0 : &X;
  p2 = &X;
  k = G ? 0 : &X;
  p3 = k;
  return k;
}


/* Macro to test the non-reduction of a volatile expression [v]. */
#define do_not_reduce_volatile(v)                                   \
  if (t[v] != v) Frama_C_show_each_v(v);                            \
  if (t[v+1] != v+1) Frama_C_show_each_v_plus(v+1);                 \
  if (t[a[v]] != a[v]) Frama_C_show_each_a(a[v]);                   \
  if (t[a[v]-1] != a[v]-1) Frama_C_show_each_a_minus(a[v]-1);       \
  if (t[a[v]] != v) Frama_C_show_each_av(v)

// Assertion that can be true only if v is properly seen as volatile
#define do_not_reduce_volatile_logic(v) \
  assert NORED: (v) == 1 && (v) == 2

/* Tests the non-reduction of volatile expressions (expression containing the
   dereference of a volatile location) during the backward propagation of
   an evaluation. */
void main3 () {
  int t[1] = {0};
  int a[2] = {0,1};

  /* The idea of each test is the same:
     in the evaluation of the condition t[x] != x, the access to t[x] may try
     to reduce the value of x to 0, and thus make the condition false.
     In all the lines below, x is a volatile or an expression whose value
     depends on a volatile, thus its reduction is prevented, and the
     following Frama_C_show_each appears on the log. Otherwise, the branch
     is dead, which is a bug. */

  /* Volatile variable */
  volatile int v = 42;
  do_not_reduce_volatile(v);
  //@ do_not_reduce_volatile_logic(v);

  /* Pointer to volatile variable */
  volatile int *v_ptr = &v;
  do_not_reduce_volatile(*v_ptr);
  //@ do_not_reduce_volatile_logic(*v_ptr);

  /* Volatile structure. */
  volatile struct vol {
    int f[1];
  } svol;
  svol.f[0] = 42;
  do_not_reduce_volatile(svol.f[0]);
  //@ do_not_reduce_volatile_logic(svol.f[0]);

  /* Pointer to volatile structure. */
  volatile struct vol *svol_ptr = &svol;
  do_not_reduce_volatile(svol_ptr->f[0]);
  //@ do_not_reduce_volatile_logic(svol_ptr->f[0]);

  /* Non volatile structure with a volatile field. */
  struct deepvol {
    volatile int g[1];
  } sdeepvol;
  sdeepvol.g[0] = 42;
  do_not_reduce_volatile(sdeepvol.g[0]);
  //@ do_not_reduce_volatile_logic(sdeepvol.g[0]);

  /* Array of volatile structs. */
  volatile struct vol volt[1] = {svol};
  do_not_reduce_volatile(volt[0].f[0]);
  //@ do_not_reduce_volatile_logic(volt[0].f[0]);

  /* Array of structs with a volatile field. */
  struct deepvol deepvolt[1] = {sdeepvol};
  do_not_reduce_volatile(deepvolt[0].g[0]);
  //@ do_not_reduce_volatile_logic(deepvolt[0].g[0]);
}



void main() {
  main1();
  main2();
  main3();
}
