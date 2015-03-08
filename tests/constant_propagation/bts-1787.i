/* run.config
OPT: -journal-disable -scf -scf-allow-cast
OPT: -journal-disable -scf
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

void g(S2_t const* q) {
  S1_t *s1 = (S1_t *)(q->p1);  /* incorrect to inline because of const qualifier */
  (*(q->p3)).s = (*(q->p1)).s + (*(q->p2)).s  ;
  //  (*(q->p1)).s += (*(q->p1)).s; /* statement to be rejected by the C typechecker */ 

  s1->s = 3;
}


int main(int c) {
  int s = c ? 1 : 2;
  G1.c12.s = s;
  g(&G2);
  return G1.c3.s;
}
int a = 0;
int b = a;
