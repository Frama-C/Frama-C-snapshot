/* run.config*
   OPT: -val @VALUECONFIG@ -deps -calldeps -inout -slevel 5 -value-msg-key malloc
*/
//@ assigns \result \from \nothing;
void *Frama_C_malloc_fresh(unsigned long n);
//@ assigns \result \from \nothing;
void *Frama_C_malloc_fresh_weak(unsigned long n);
//@ assigns \result \from \nothing;
void *Frama_C_malloc_by_stack(unsigned long n);

volatile int v;

void g(int *p, int k) { p[k] = k; }

void main(int i, int j) {
  int *p, *q;
  p = Frama_C_malloc_fresh_weak(100);
  *p = i;
  *p = j; // Cannnot perform strong update for deps, variable is weak

  q = Frama_C_malloc_fresh(100);
  *q = i;
  *q = j; // Can perform strong update for deps


  int *r;
  for (int l=0; l<10; l++) {
    r = Frama_C_malloc_by_stack((l+1)*4);
    g(r, l+v); // Again, we can only perform weak updates (after iteration 1)
  }
}
