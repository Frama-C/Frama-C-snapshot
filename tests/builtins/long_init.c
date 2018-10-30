/* run.config*
   DONTRUN: tests run by Longinit_sequencer.i
*/

#include <stdlib.h>

#define N1 10
#define N2 50
#define N3 10

volatile int nondet;

int a1[N1];
struct st {
  unsigned long t[N2];
  double d[N3];
} stuff;

double subanalyze(double *d) {
  return *d < 15 ? 1.0 : *d / 15.0;
}

double analyze(int *a, unsigned long *b, double *c) {
  int i;
  double res = 0.0;
  /*@ slevel 5; */
  for (i = 0; i < 5; i++) {
    res += a[i + 3] + b[i * 2] + c[i + 1];
    res += subanalyze(&c[i + 1]);
  }
  return res;
}

char garbled_mix = "abc";
char *s = "abc";
//int another_global = 42; // from init_global2.c
//int yet_another_global = 43; // from init_global3.c
double *pr, *pr2, *pr_escaping, **ppr;
int *alloc1, *alloc2, *alloc3;

double dmin(double *pd1, double *pd2) {
  if (*pd1 < *pd2) return *pd1;
  else return *pd2;
}

int fun(int k) {
  return k+1;
}

typedef int (*i_fp_i)(int);
i_fp_i fp = &fun;

/*@ assigns a1[..], stuff, pr, pr2, pr_escaping, alloc1, alloc2
      \from \nothing; */
void init_inner(int n, char const *tea) {
  int i;
  /*@ slevel N3; */
  for (i = 0; i < N1; i++) {
    a1[i] = i;
  }
  for (i = 0; i < N2; i++) {
    stuff.t[i] = a1[i/5] + 3;
  }
  for (i = 0; i < N3; i++) {
    stuff.d[i] = 3.125 * i;
  }
  /*@ slevel 0; */
  double r = analyze(a1, stuff.t, stuff.d);
  double r2 = analyze(a1, stuff.t+1, stuff.d+1);
  pr = nondet ? &r : &r2;
  pr2 = &r2;
  pr_escaping = &r2;
  alloc1 = malloc(sizeof(int*));
  *alloc1 = alloc1;
  alloc2 = malloc(2*sizeof(int));
  *alloc2 = 37;
  free(alloc2);
}

int inited;

/*@ assigns a1[..], stuff, pr, pr2, pr_escaping, alloc1, alloc2, inited
      \from \nothing; */
void init_outer() {
    init_inner(13, "tea");
    inited = 1;
}

int main() {
  init_outer();
  char *sa = s;
  Frama_C_dump_each();
  double r = analyze(a1, stuff.t, stuff.d);
  double r2 = analyze(a1, stuff.t+1, stuff.d+1);
  pr = nondet ? &r : &r2;
  pr2 = nondet ? &r : &r2;
  ppr = nondet ? &pr : &pr2;
  double dm = dmin(pr, *ppr);
  int res_from_fp = (*fp)(31);
  int res = (int)r % 256;
  *alloc1 = inited;
  int local = *alloc1;
  free(alloc1);
  alloc3 = malloc(sizeof(int));
  //local = another_global; // from init_global2.c
  //int local2 = yet_another_global; // from init_global3.c
  return 0;
}
