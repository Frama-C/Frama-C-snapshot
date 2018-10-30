/* run.config
   COMMENT: ranges in a few builtins
*/
#include "stdlib.h"
/*@ requires !\valid(s + (3..n+1000)); */
void f(char *s, long n){}
/*@ requires \valid(ptr + (0 .. size - 1));
    ensures ! \valid(ptr + (0 .. size + 1));
    // In pure ACSL, the following predicate is true;
    // however at runtime, its evalulation results in UB ==> false.
    // ensures  ! \valid(ptr + (0 .. SIZE_MAX*SIZE_MAX)); */
void g(long *ptr, size_t size) { }
extern void *malloc(size_t p);
extern void free(void* p);
struct S { int a[2]; float *b; float *c;};
int main(void) {
  int *a;
  a  = malloc(10*sizeof(int));
  /*@ assert \valid(a + (0 .. 4)); */ ;
  int j = 2;
  /*@ assert \valid(a + (4 .. 8+j)); */ ;
  /*@ assert !\valid(a + (10 .. 11)); */ ;
  free(a);

  char *b;
  b  = malloc(10*sizeof(char));
 /*@ assert \valid(b + (0 .. 10)); */ ;
 /*@ assert !\valid(b + (11 .. 15)); */ ;

  long t[3] = {7l, 8l, 9l};
  /*@ assert \valid(&t[0..2]); */ ;
  /*@ assert !\valid(&t[3..5]); */ ;
  g(t, 3);

  double t2[4];
  t2[0] = 0.5;
  t2[1] = 1.5;
  /*@ assert \initialized(&t2[0..1]); */ ;
  /*@ assert !\initialized(&t2[2..3]); */ ;

  /*@ assert !\initialized(b + (0 .. 10));*/
  free(b);

  int n = 2;
  float t3[7][2][4];
  /*@ assert !\initialized(&t3[(n-1)..(n+2)][1][0..1]); */ ;

  /*@ assert !\valid_read(&t3[6][1][0] + (2..10)); */
  /*@ assert \valid_read(&t3[(n-1)..(n+2)][1]); */

  struct S s;
  s.a[0] = 7; s.a[1] = 8;
  /*@ assert \initialized(&s.a[0] + (1..2)); */ ;
  /*@ assert !\initialized(s.b + (0..1)); */ ;

  int **multi_dynamic;
  int size1 = 5, size2 = 9;
  multi_dynamic = malloc(size1 * sizeof(*multi_dynamic));
  int i;
  for(i = 0; i < size1; i++) {
    multi_dynamic[i] = malloc(size2 * sizeof(*(multi_dynamic[i])));
  }
  /*@ assert \valid(&multi_dynamic[4][1..7]); */ // single call to builtin
  /*@ assert \valid(&multi_dynamic[2..4][1..7]); */ // need to modify Mmodel
                                                    // => not_yet
  for(i = i-1 ; i >= 0 ; i--) {
    free(multi_dynamic[i]);
  }
  free(multi_dynamic);

  char c = 'w';
  f(&c, 5);
}