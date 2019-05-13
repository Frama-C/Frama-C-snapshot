/* run.config*
   STDOPT: #"-eva-builtin realloc:Frama_C_realloc -eva-mlevel 0 -inout-callwise -inout-no-print "
*/
#include <stdlib.h>

volatile int nondet;

void fill(char *b, int n) {
  //@slevel 10000;
  for (int i = 0; i < n; i++) {
    b[i] = i;
  }
  //@slevel default;
  ;
}

void fill2(char *b, int n) {
  //@slevel 10000;
  for (int i = 0; i < n; i++) {
    b[i] = 10*i;
  }
  //@slevel default;
  ;
}

void test_exact_null() {
  char *p = realloc(NULL, 10);
  if (p) fill(p, 10);
}

void test_exact_null_free() {
  char *p = realloc(NULL, 0);
}

void test_exact_nonnull_expand() {
  void *p1 = malloc(5);
  if (p1) fill(p1, 5);
  char *p2 = realloc(p1, 10);
  if (p2) fill(p2, 10);
  else if (p1) fill(p1, 5); // realloc failed - p1 should not have changed
}

void test_exact_nonnull_shrink() {
  void *p1 = malloc(5);
  if (p1) fill(p1, 5);
  char *p2 = realloc(p1, 2);
  if (p2) fill(p2, 2);
}

void test_exact_nonnull_free() {
  void *p1 = malloc(5);
  if (p1) fill(p1, 5);
  char *p2 = realloc(p1, 0);
}

void test_maybe_nonnull() {
  void *p1 = nondet ? NULL : malloc(5);
  char *p2 = realloc(p1, 2);
  if (p2) fill(p2, 2);
}

void test_same_size() {
  void *p1 = malloc(5);
  if (p1) fill(p1, 5);
  char *p2 = realloc(p1, 5); if (p2) { *p2 = 0; }
}

void test_imprecise_size() {
  size_t s = nondet ? 1 : 2;
  char *p = realloc(NULL, s);
  if (p) fill(p, s);
}

void test_imprecise_size_but_precise_fill() {
  size_t s = nondet ? 1 : 2;
  char *p = realloc(NULL, s);
  if (p) fill(p, 1);
}

void test_imprecise_size_free() {
  void *p1 = malloc(5);
  if (p1) fill(p1, 5);
  size_t s = nondet ? 0 : 2;
  char *p2 = realloc(p1, s);
  if (p2) fill(p2, s);
}

void test_imprecise_both() {
  void *p1 = nondet ? NULL : malloc(5);
  size_t s = nondet ? 0 : 2;
  char *p2 = realloc(p1, s);
  if (p2) fill(p2, s);
}

void test_possibly_invalid_realloc() {
  char *p1 = malloc(5);
  if (!p1) return;
  int offset = nondet ? 0 : (nondet ? 10 : 20);
  char *p2 = realloc(p1 + offset, 2);
  if (p2) fill(p2, 2);
}

void test_invalid_realloc() {
  char *p1 = malloc(5);
  if (!p1) return;
  char *p2 = realloc(p1+1, 2);
  if (p2) fill(p2, 2);
}

void test_invalid_realloc2() {
  char *p = realloc((void*)100, 2);
  if (p) fill(p, 2);
}

void test_invalid_realloc3() {
  char *p1 = malloc(5);
  if (!p1) return;
  int offset = nondet ? 10 : (nondet ? 20 : 30);
  char *p2 = realloc(p1 + offset, 2);
  if (p2) fill(p2, 2);
}

void test_realloc_sequence() {
  void *p1 = malloc(2);
  char *p2 = realloc(p1, 3);
  char *p3 = realloc(p2, 5);
  if (p3) fill(p3, 5);
}

void test_realloc_loop() {
  void *p1 = malloc(10);
  if (!p1) return;
  char *p2 = p1;
  if (p2) fill(p2, 5);
  else return;
  //@ slevel 32;
  for (int i = 0; i < 5; i++) {
    char *p3 = realloc(p2, 10+5*i);
    if (!p3) { /* could not reallocate */
      fill(p2, 10); // checks that the previous pointer is still valid
    } else {
      fill(p3+10+5*(i-1), 5);
      p2 = p3;
    }
  }
  //@ slevel default;
  ;
}

void test_realloc_multiple_bases() {
  void *p1 = malloc(2);
  char *p2;
  if (nondet) {
    p2 = malloc(3);
  } else {
    p2 = realloc(p1, 3);
  }
  char *p3 = realloc(p2, 5);
  if (p3) fill(p3, 5);
}

void test_realloc_multiple_bases2() {
  void *p1 = malloc(4);
  if (!p1) return;
  fill(p1, 4);
  char *p2 = nondet ? realloc(p1, 6) : malloc(2);
  if (!p2) return;
  fill2(p2, 2);
  char *p3 = realloc(p2, 10);
  if (!p3) return;
  char *p4 = realloc(p3, 5);
  if (!p4) return;
  fill(p4, 5);
  char *p5 = malloc(6);
  if (!p5) return;
  fill2(p5, 3);
  char *p6 = realloc(nondet ? p4 : p5, 4);
  if (!p6) return;
  fill(p6, 4);
}

void test_realloc_multiple_bases_loop() {
  size_t size = 10;
  char *p = malloc(size);
  for (int i = 0; i < 10; i++) {
    size_t new_size = nondet ? size : 10+2*i; 
    p = nondet ? realloc(p, new_size) : p;
    if (!p) return;
    fill(p, new_size);
  }
}

int main(){
  test_exact_null();
  test_exact_null_free();
  test_exact_nonnull_expand();
  test_exact_nonnull_shrink();
  test_exact_nonnull_free();
  test_maybe_nonnull();
  test_same_size();
  test_imprecise_size();
  test_imprecise_size_but_precise_fill();
  test_imprecise_size_free();
  test_imprecise_both();
  test_possibly_invalid_realloc();
  if (nondet) test_invalid_realloc();
  if (nondet) test_invalid_realloc2();
  if (nondet) test_invalid_realloc3();
  test_realloc_sequence();
  test_realloc_loop();
  test_realloc_multiple_bases();
  test_realloc_multiple_bases2();
  test_realloc_multiple_bases_loop();
  return 0;
}
