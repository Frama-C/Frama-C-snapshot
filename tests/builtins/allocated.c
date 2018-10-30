/* run.config*
   STDOPT: +"-slevel 1 -val-mlevel 0"
   STDOPT: +"-slevel 999 -val-builtin malloc:Frama_C_malloc_fresh,__fc_vla_alloc:Frama_C_malloc_fresh,__fc_vla_free:Frama_C_vla_free"
*/
#define assert_bottom(exp) if (nondet) {exp; Frama_C_show_each_unreachable();}

#ifndef __FRAMAC__
#include <stdio.h>
#define Frama_C_show_each_unreachable(...)
#define Frama_C_show_each_p(...)
#define Frama_C_show_each_p0(...)
#define Frama_C_show_each_p_after_free(...)
#define Frama_C_show_each_p1(...)
#define Frama_C_show_each_p2(...)
#define Frama_C_show_each_pj(e) printf("pj = %d\n", e);
#endif

#include <stdlib.h>
volatile int nondet;
int main() {
  int i, j;
  int *p;

  // constant size
  p = malloc(4); //@ assert \block_length(p) == 4;
  *p = 17; *p = 18;
  assert_bottom(p[1]);
  assert_bottom(p[2]);
  Frama_C_show_each_p(p);
  Frama_C_show_each_p0(p[0]); // after a weak update, p[0] IN {17, 18, UNINIT}
  free(p);
  Frama_C_show_each_p_after_free(p);

  // non-constant size, but constant
  int k = nondet ? 8 : 8;
  p = malloc(k); //@ assert \block_length(p) == 8;
  p[0] = 13;
  p[1] = 42;
  p[1] = 54;
  Frama_C_show_each_p(p);
  Frama_C_show_each_p0(p[0]);
  Frama_C_show_each_p1(p[1]);
  if (nondet) free(p);
  p[0] = 41; // must emit danglingness alarm
  free(p); // should emit double-free alarm
  assert_bottom(p[0] = 43); // strong free should result in bottom here

  // variable size
  k = nondet ? 8 : 12;
  p = malloc(k); //@ assert \block_length(p) >= 8 && \block_length(p) <= 12;
  p[0] = 13;
  p[1] = 42;
  p[2] = 77; // must emit alarm
  Frama_C_show_each_p(p);
  Frama_C_show_each_p0(p[0]);
  Frama_C_show_each_p1(p[1]);
  Frama_C_show_each_p2(p[2]);
  free(p);

  // loop with constant size, enough slevel => no alarms
  //@ slevel 1000;
  for (i = 0; i < 4; i++) {
    p = malloc(sizeof(int)); //@ assert \block_length(p) == sizeof(int);
    *p = i;
    Frama_C_show_each_p(p);
    Frama_C_show_each_p0(p[0]);
    free(p);
  }
  //@ slevel default;

  // loop with constant size, not enough slevel => possible alarms
  for (i = 0; i < 4; i++) {
    p = malloc(sizeof(int));
    *p = i;
    Frama_C_show_each_p(p);
    Frama_C_show_each_p0(p[0]);
    free(p);
  }

  // loop with variable size, possible leak
  for (i = 0; i < 4; i++) {
    p = malloc(sizeof(int) * i); /*@ assert \block_length(p) >= 0 &&
                                     \block_length(p) <= sizeof(int) * 4; */
    for (j = 0; j < i; j++) {
      if (nondet) p[j] = 7 * i + 3 * j;
    }
    if (nondet) free(p); // possible alarm about freeability
  }

  //@ slevel merge;
  p = malloc(0); //@ assert \block_length(p) == 0;
  free(p);


  unsigned int size = nondet;
  //@ assert Assume: size <= 100;
  p = malloc(size); Frama_C_show_each(p);
  *p = 0;
  struct bitf {
    char i1: 1;
    char i2: 1;
    char i3: 1;
    char i4: 1;
    char i5: 1;
    char i6: 1;
    char i7: 1;
    char i8: 1;
  };
  struct bitf *pb = p;
  Frama_C_show_each(pb->i1); // Alarm
  free(p);

  //@ assert size >= 1;
  p = malloc(size); Frama_C_show_each(p);
  *p = 0;
  pb = p;
  Frama_C_show_each(pb->i1); // No alarm
  free(p);

  p = malloc(0); Frama_C_show_each(p);
  pb = p;
  if (nondet) {
    Frama_C_show_each(pb->i1); // Alarm
  }
  free (p);
  for (i = 0; i < 10; i++) {
    int a[i+1];
    for (j = 0; j <=i; j++) {
      a[j] = j;
    }
    Frama_C_show_each(a[i]);
  }
  return 0;
}
