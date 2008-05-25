/* run.config
   GCC:
   OPT: -security-analysis -lib-entry f -slevel 4
   OPT: -security-analysis -lib-entry g
   */

//#define GCC
#ifdef GCC
  #include <stdlib.h>
  #include <stdio.h>
#else
  #define FRAMA_C_MALLOC_INDIVIDUAL
  #include <share/malloc.c>
#endif

int *x;
int t[4];

/*@ requires security_status(x) == public(); */
void send(int x);

/* ************************************************************************* */

void f() {
  int i;
  x = malloc(sizeof(int));

  for (i = 0; i < 4; i++) 
    t[i] = i;

  *(t+3) = (int /*@ public */) *(t+2);
  *x = t[3];

// see BTS#273
/*   while (i>0) { */
/*     i--; */
/*     send(t[i]); */
/*   } */

  while (i>0) {
    i--;
  }

  send(t[0]);
  send(t[1]);
  send(t[2]);
  send(t[3]);

  send(*x);
}

/* ************************************************************************* */

void sub_g(int *p) {
  int *q = p;
  send(*q);
}

void g() {
  x = malloc(sizeof(int));
  *x = (int /*@ public */) 1;
  sub_g(x);
  t[3] = (int /*@ public */) 2;
  sub_g(&*(t+2)+1);
}

/* ************************************************************************* */
