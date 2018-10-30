/* run.config*
   STDOPT: #"-eva-warn-key garbled-mix"
*/

/* Test the soundness and the precision of backward reductions for
   the addition and subtraction of pointers and integers
   (binop PlusPI and MinusPI of Cil). */

#include <stdint.h>


volatile uintptr_t rand; /* Use as undeterminism. */

/* Reduction by the valid part of the location (p + q), where p and q are precise
   pointers or integers.
   tests the backward propagation of this reduction into p and q. */
void main1 () {
  int a = 0, b = 1;
  int *p = rand ? &a : (int*)rand;
  int *q = rand ? &b : (int*)rand;
  int v; char w;
  if (rand) {
    /* According to the C semantics, computes (p + sizeof(*p) q),
       so the b address from q is lost in the operation. */
    v = *((p + (uintptr_t)q));
    Frama_C_show_each_only_a(v, p, q);
  }

  if (rand) {
    /* Here, sizeof(*p) = 1, so &a+[0..3] and &b+[0..3] are both valid pointers
       resulting from the operation, and no address can be lost. */
    w = *(((char *)p + (uintptr_t)q));
    Frama_C_show_each_reduced_offset(w, p, q);
  }

  if (rand) {
    /* (q / 4) is a garbled mix, so no reduction is feasible. */
    v = *((p + ((uintptr_t)q / 4)));
    Frama_C_show_each_no_reduction(v, p, q);
  }

}


/* Reduction of the resulting location according to the pointed value of (p + q),
   where p and q are both precise pointers or integers.
   tests the backward propagation of this reduction into p and q. */
void main2 () {
  int a = 0, b = 1;
  int *p = rand ? &a : (int*)rand;
  int *q = rand ? &b : (int*)rand;

  if (rand)
    if (*( (int *) ((char*)p + (uintptr_t)q)) == 0)
      Frama_C_show_each_int_if(p, q);
    else
      Frama_C_show_each_int_else(p, q);

  if (rand)
    if (*( (char *) ((char*)p + (uintptr_t)q)) == 0)
      Frama_C_show_each_char_if(p, q);
    else
      Frama_C_show_each_char_else(p, q);
}


/* Garbled Mix creator since 1987. */
int* gm(int *p) { return (int *) ((uintptr_t) p * 2 / 2); }


/* Backward propagation of a reduction on (p + q), where p is a garbled mix,
   and q a precise pointer or an integer. */
void main3 () {
  int a = 0, b = 1, c = 2;
  int *p = gm (&a);
  int *q = rand ? &b : (int*)rand;
  int v; char w;
  if (rand) {
    /* According to the C semantics, computes (p + sizeof(*p) q),
       so the b address from q is lost in the operation. */
    v = *((p + (uintptr_t)q));
    Frama_C_show_each_GM_only_a(p, q);
  }

  if (rand) {
    /* Here, size = 0, so &b+[0..3] is valid, and p is reduced accordingly. */
    w = *(((char*)p + (uintptr_t)q));
    Frama_C_show_each_GM_reduce_p_offset(p, q);
  }

  p = gm (&a);
  q = &b;

  if (rand) {
    /* The same but q is only a pointer, so &a is impossible too. */
    v = *((p + (uintptr_t)q));
    Frama_C_show_each_GM_BOTTOM(p, q);
  }

  p = gm (rand ? &a : &b);
  q = rand ? &b : (int*)rand;

  if (rand) {
    /* Here, &b appear in the garbled mix of p, and may interfere with the
       (4 * &b) from q: no reduction is feasible. */
    v = *((p + (uintptr_t)q));
    Frama_C_show_each_GM_no_reduction(p, q);
  }

  p = gm (rand ? &a : &b);
  q = rand ? &c : (int*)rand;

  if (rand) {
    /* &c is the only valid location after the condition, so q = &c and p = 0. */
    if ( *(((char*)p + (uintptr_t)q)) == 2)
      Frama_C_show_each_GM_only_c(p, q);
  }
  if (rand) {
    /* &b is the only valid location after the condition, so p = &b+[..] and
       q is an integer. */
    if ( *((p + (uintptr_t)q)) == 1)
      Frama_C_show_each_GM_only_b(p, q);
  }

  p = gm (rand ? &a : &b);
  q = rand ? &b : (int*)rand;

  if (rand) {
    /* No pointer on c, so bottom after the condition. */
    if ( *(((char*)p + (uintptr_t)q)) == 2)
      Frama_C_show_each_GM_BOTTOM(p, q);
  }
  if (rand) {
    /* &b is the only valid location after the condition, but it may be builds
       by any combination of a garbled mix of &b for p. */
    if ( *(((char*)p + (uintptr_t)q)) == 1)
      Frama_C_show_each_GM_only_b_and_gm(p, q);
  }
}


/* Backward propagation of a reduction on (p + q) when p and q are garbled mix. */
void main4() {
  int a = 0, b = 1, c = 2;
  int *p = gm (rand ? &a : &b);
  int *q = gm (&c);
  int v; char w;
  if (rand) {
    /* Garbled mixs on both sides, no reduction. */
    v = *((p + (uintptr_t)q));
    Frama_C_show_each_2GM_no_reduction(p, q);
  }

  if (rand) {
    /* &b is the only valid location after the condition. */
    if ( *(((char*)p + (uintptr_t)q)) == 1)
      Frama_C_show_each_2GM_only_b(p, q);
  }

  p = gm (rand ? &a : &b);
  q = gm (rand ? &b : &c);

  if (rand) {
    /* &b is the only valid location after the condition. */
    if ( *(((char*)p + (uintptr_t)q)) == 1)
      Frama_C_show_each_2GM_TEST(p, q);
  }
  if (rand) {
    /* &a is the only valid location after the condition, but it may be
       obtained by a combination between GM(&a, &b) and GM(&b). */
    if ( *((p + (uintptr_t)q)) == 0)
      Frama_C_show_each_2GM_gm_of_a_b(p, q);
  }
  if (rand) {
    /* &c is the only valid location after the condition, but it may be
       obtained by a combination between GM(&b) and GM(&b,&c). */
    if ( *(((char*)p + (uintptr_t)q)) == 2)
      Frama_C_show_each_2GM_gm_of_b_c(p, q);
  }
}


int main() {

  main1();
  main2();
  main3();
  main4();

  /* a = 0; */
  /* b = 1; */
  /* c = 2; */

  return 0;

}
