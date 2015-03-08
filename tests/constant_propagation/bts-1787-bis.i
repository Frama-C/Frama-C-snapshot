/* run.config
OPT: -journal-disable -print
OPT: -journal-disable -scf
OPT: -journal-disable -scf -scf-allow-cast
*/

typedef struct {
   int   s;
} S1_t;

typedef struct {  
  const S1_t* p1;
  S1_t* p2;
  S1_t* p3;
} S2_t;

struct {
  S1_t c12;
  S1_t c3;
} G1;

static S2_t const G2 = {
  &G1.c12,
  &G1.c12,
  &G1.c3
};

void h1(S2_t const* q) {
  S1_t *s1 = q->p1; 
  // gcc warning: initialization discards ‘const’ qualifier from pointer target type [enabled by default]
}
void h2(S2_t const* q) {
  S1_t *s2 = (S1_t *)(q->p1); // same AST than h1 (cast explicited by the kernel) for the right-value
}
void h3(S2_t const* q) {
  S1_t *s3 = (S1_t *)G2.p1;
}
void h4(S2_t const* q) {
  S1_t *s5 = (S1_t const *)(& G1.c12);                        
  // gcc warning: initialization discards ‘const’ qualifier from pointer target type [enabled by default]
}
void h5(S2_t const* q) {
  S1_t *s6 = & G1.c12; // same AST than h4 (cast removed by the kernel) for the right-value
}
// The result for h1 to h5 can be &G1.c12 for both value of the option -scf-allow-cast

int main(int c) {
  int s = c ? 1 : 2;
  G1.c12.s = s;
  h1(&G2);
  h2(&G2);
  h3(&G2);
  h4(&G2);
  h5(&G2);
  return G1.c3.s;
}
