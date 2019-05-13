/* run.config*
   STDOPT: +"-slevel 10 -eva-builtin malloc:Frama_C_malloc_fresh,realloc:Frama_C_realloc_multiple -eva-malloc-functions malloc,realloc"
   STDOPT: +"-slevel 10 -eva-builtin malloc:Frama_C_malloc_fresh,realloc:Frama_C_realloc_multiple -eva-malloc-functions malloc,realloc -eva-alloc-returns-null"
*/
#include <stdlib.h>
#include "__fc_builtin.h"

void main1() {
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
  free (s);
}

void main2() {
  int *q = (int *) malloc(5*sizeof(int));
  for (int i=0;i<5;i++){q[i]=7;}

  int *r = (int *) malloc(6*sizeof(int));
  for (int i=0;i<6;i++){r[i]=8;}

  int *p;
  int x=Frama_C_interval(0,2);
  if (x != 0){p=r;}else{p=q;}
  if (x == 2) p = NULL;

  //@ slevel merge;

  Frama_C_dump_each();

  int * s=(int *) realloc(p,7*sizeof(int));

  Frama_C_dump_each();
  free (s);
}

void main3() {
  int **p = malloc(sizeof(int *));
  int **q = malloc(sizeof(int *));
  int **r, **s;
  {
    int x, y;
    *p = &x;
    *q = &y;
    int c = Frama_C_interval (0, 2);
    if (c == 0) r = NULL;
    else if (c == 1) r = p;
    else r = q;
    //@ slevel merge;
    Frama_C_dump_each();
    s = realloc(r, 2 * sizeof(int *));
    **s = 17;
    Frama_C_dump_each();
  }
  Frama_C_dump_each(); // &x must no appear in q[..]
  free(q);
}


void main(int v) {
  if (v == 1) main1();
  else if (v == 2) main2();
  else if (v == 3) main3();
}

