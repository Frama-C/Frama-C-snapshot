/* run.config*
   OPT: -eva @EVA_CONFIG@ -eva-memexec -deps -inout -eva-mlevel 0
*/

//@ assigns \result;
void *Frama_C_malloc_fresh(unsigned long n);

//@ assigns \result;
void *Frama_C_malloc_fresh_weak(unsigned long n);


void f(int *p, int i) {
  *p = i;
}

volatile v;

void main() {
  int *p = Frama_C_malloc_fresh (4);
  if (v) {
    f(p, 2);
    f(p, 1); // This call or the corresponding one below could be cached. It is not, because we forbid memexec to take full updates to a strong variable into account for malloced bases, because they may become weak later
  } else {
    f(p, 1);
  }

  int *q = Frama_C_malloc_fresh_weak (4);
  if (v) {
    f(q, 2);
    f(q, 1);
  } else {
    f(q, 1); // This call cannot be cached; since q is weak, f(q, i)
             // actually depends on *q
  }
}
