/* run.config*
   STDOPT: +"-eva-auto-loop-unroll 10"
   STDOPT: +"-eva-auto-loop-unroll 128"
*/

/* Tests the automatic loop unrolling heuristic. */

#include <__fc_builtin.h>

volatile int undet;

int g = 0;
void incr_g () {
  g++;
}

int incr (int i) {
  return i+1;
}

void simple_loops () {
  int res = 0;
  /* This loop should be automatically unrolled on the second run. */
  for (int i = 0; i < 100; i++) {
    res++;
  }
  Frama_C_show_each_auto(res);
  res = 0;
  /* This loop should not be automatically unrolled. */
  for (int i = 0; i < 1000; i++) {
    res++;
  }
  Frama_C_show_each_imprecise(res);
  res = 0;
  /* The annotation has priority over the automatic loop unrolling:
     this loop should never be unrolled. */
  /*@ loop unroll 0; */
  for (int i = 0; i < 100; i++) {
    res++;
  }
  Frama_C_show_each_imprecise(res);
  res = 0;
  /* The annnotation has priority over the automatic loop unrolling:
     this loop should always be unrolled. */
  /*@ loop unroll 100; */
  for (int i = 0; i < 100; i++) {
    res++;
  }
  Frama_C_show_each_singleton(res);
}

/* Examples of various loops that should be automatically unrolled
   on the second run, but not on the first. */
void various_loops () {
  int res = 0;
  /* Decreasing loop counter. */
  for (int i = 64; i > 0; i--)
    res++;
  Frama_C_show_each_64(res);
  res = 0;
  /* Decrements the loop counter by 3. */
  for (int i = 120; i > 0; i -= 3)
    res++;
  Frama_C_show_each_40(res);
  res = 0;
  /* Several increments of the loop counter. */
  for (int i = 0; i < 160; i++) {
    i += 2;
    res++;
    i--;
  }
  Frama_C_show_each_80(res);
  res = 0;
  /* Random increments of the loop counter. */
  for (int i = 0; i < 160;) {
    res++;
    if (undet)
      i += 2;
    else
      i += 5;
  }
  Frama_C_show_each_32_80(res);
  res = 0;
  /* Other loop breaking condition. */
  for (int i = 0; i < 111; i++) {
    res++;
    if (undet && res > 10)
      break;
  }
  Frama_C_show_each_11_111(res);
  res = 0;
  /* More complex loop condition. */
  int x = 24;
  int k = Frama_C_interval(0, 10);
  for (int i = 75; i + x > 2 * k; i -= 2)
    res++;
  Frama_C_show_each_40_50(res);
  res = 0;
  /* Loop calling some functions that do not modify the loop counter. */
  for (int i = 0; i < 25; i++) {
    incr_g();
    int t = incr(i);
    res = incr(res);
  }
  Frama_C_show_each_25(res);
  res = 0;
  /* Nested loops. */
  res = 0;
  for (int i = 0; i < 16; i++) {
    for (int j = 0; j < i; j++) {
      res++;
    }
  }
  Frama_C_show_each_120(res);
  res = 0;
}

/* Loops that cannot be unrolled. */
void complex_loops () {
  /* Loop counter modified through a pointer. */
  int res = 0;
  int i = 0;
  int *p = &i;
  while (i < 64) {
    (*p)++;
    res++;
  }
  Frama_C_show_each_imprecise(res);
  /* Loop counter modified within a nested loop. */
  res = 0;
  i = 0;
  while (i < 64) {
    for (int j = 0; j < i; j++) {
      i++;
    }
    res++;
    i++;
  }
  Frama_C_show_each_imprecise(res);
  /* Loop counter incremented under a condition. */
  res = 0;
  i = 0;
  while (i < 10) {
    if (undet)
      i++;
    res++;
  }
  Frama_C_show_each_imprecise(res);
  res = 0;
  i = 0;
  while (i < 10) {
    if (undet)
      i++;
    else
      i++;
    res++;
  }
  Frama_C_show_each_imprecise(res);
  /* Loop counter modified by a function. */
  res = 0;
  g = 0;
  while (g < 64) {
    incr_g();
    g++;
    res++;
  }
  Frama_C_show_each_imprecise(res);
  res = 0;
  /* Too complex loop condition. */
  int t[10] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
  i = 0;
  while (t[i] < 6) {
    i++;
    res++;
  }
  Frama_C_show_each_imprecise(res);
  res = 0;
  /* Random loop condition. */
  i = 0;
  while (i < 64 && undet) {
    i++;
    res++;
  }
  Frama_C_show_each_imprecise(res);
}


void main () {
  simple_loops ();
  various_loops ();
  complex_loops ();
}
