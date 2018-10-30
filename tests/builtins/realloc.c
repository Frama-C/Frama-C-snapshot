/* run.config*
   STDOPT: +"-slevel 10 -eva-builtin malloc:Frama_C_malloc_fresh -eva-malloc-functions malloc,realloc -eva-warn-copy-indeterminate @all"
*/

#include <stdlib.h>
#include "__fc_builtin.h"

/*@ assigns ((char*)\result)[0..s-1] \from ((char*)p)[0..s-1]; */
void *Frama_C_realloc(void *p, size_t s);

void main1(){
  int *p = malloc(sizeof(int));
  *p = 17;
  int *pp = p;
  Frama_C_dump_each();
  int *q = realloc(p, 2 * sizeof(int));
  Frama_C_dump_each();
  free (q);
}

void main2() {
  int x=Frama_C_interval(3,4);
  int *r = (int *) malloc(x*sizeof(int));
  for (int i=0;i<x;i++){r[i]=6;}
  //@ slevel merge;
  int * s= realloc(r,6*sizeof(int));
  Frama_C_dump_each();
  free(s);
}

void main3() {
  int *q = (int *) malloc(5*sizeof(int));
  for (int i=0;i<5;i++){q[i]=5;}

  int *r = (int *) malloc(6*sizeof(int));
  for (int i=0;i<6;i++){r[i]=6;}

  int *p;
  int x=Frama_C_interval(0,1);
  if (x != 0){p=r;}else{p=q;}

  //@ slevel merge;

  Frama_C_dump_each();

  int * s=(int *) realloc(p,7*sizeof(int));

  Frama_C_dump_each();
  free(q); free (r); free (s);
}

void main4() {
  int sizep = Frama_C_interval(2,10);
  int sizeq = Frama_C_interval(0,10);
  int *p = malloc(sizep*sizeof(int));
  int *q = malloc(sizeq*sizeof(int));
  for (int i = 0; i < 10; i++) {
    p[i] = i;
    q[i] = i;
  }
  Frama_C_dump_each();
  // p[..] and q[..] are fully initialized: if the cell is valid,
  // its value has been written (but validity alarms have been emitted)
  // The example is a bit simplistic though, because only traces with
  // a validity of 10 are possible here.

  int *rp = realloc(p, 15*sizeof(int));
  int *rq = realloc(q, 8*sizeof(int));
  Frama_C_dump_each();
  // rp and rq are partially initialized: if size is e.g. 4, realloc can copy
  // only the first 4 ints, the remainder is left unchanged
  free(rp); free(rq);
}

void main5() {
  int *p = malloc(sizeof(int));
  *p = 1;
  int c = Frama_C_interval(0, 1);
  int *q;
  if (c) { q = p; } else { q = NULL; }
  //@ slevel merge;

  Frama_C_dump_each();

  int *r = realloc(q, 2*sizeof(int));
  Frama_C_dump_each();

  free(p); free(r);
}

void main6() {
  int c = Frama_C_interval(0, 10);
  int *m = malloc(sizeof(int) * 2);
  if (c) {
    int x = 1;
    int *p;
    if (c == 2)
      p = m+1;
    else
      p = &x;
    //@ slevel merge;
    Frama_C_show_each(p);
    int *q = realloc(p, 2*sizeof(int)); // Always invalid, non-malloced vars
    //@ assert UNREACHED: \false;
  }
  free(m);
}

void main7() {
  int **p = malloc(sizeof(int *));
  int **q;
  {
    int x;
    *p = &x;
    q = realloc(p, 2 * sizeof(int *));
    Frama_C_dump_each();
  }
  Frama_C_dump_each(); // &x must no appear in q[..]
  free(q);
}

void main8() {
  int *p = malloc(sizeof(int) * 2);
  p[0] = 1;
  p[1] = 2;
  int *q = realloc(p, 0);
  Frama_C_dump_each();
  free (q);
}

void main9() {
  int *p = malloc(sizeof(int) * 2);
  p[0] = 1;
  p[1] = 2;
  int *q = realloc(p, 1); // Size reduction
  Frama_C_dump_each();
  free (q);
}

volatile v;

void f(int x) {
  Frama_C_show_each(x);
}

void main10() {
  int *p = malloc(sizeof(int));
  int *q;
  *p = 4;
  while (v) {
    q = p;
    p = realloc(p, 2*sizeof(int)); // the same base is reallocated. Nothing is dangling except q
    *p = *p; // always succeeds (provided realloc does not return NULL)
    Frama_C_show_each_main10(*p);
    Frama_C_dump_each();
  }
}

void main() {
  main1();
  main2();
  main3();
  main4();
  main5();
  main6();
  main7();
  main8();
  main9();
  main10();
}

