/* -------------------------------------------------------------------------- */
/* --- NON CONCLUSIVE TEST                                                --- */
/* -------------------------------------------------------------------------- */
// WP aborts because logic variable S0 appears to be typed 'void' by CIL
// BTS #428
/* -------------------------------------------------------------------------- */

typedef struct _S S;
/*@ axiomatic A {
logic S S0;
logic S S1(integer e);
logic S S2(S seq, integer e);
  
axiom A1: \forall integer e; S1(e)!=S0;
axiom A2: \forall integer e1,e2; S1(e1)==S1(e2) <==> e1==e2;
axiom A3: \forall integer e; S1(e)==S2(S0,e);
axiom A4: \forall integer e1,e2, S s1,s2; S2(s1,e1)==S2(s2,e2) <==> (s1==s2&&e1==e2);
} */

S s;

//@ ensures s==S2(\old(s),e);
extern void g(const int e);

/*@ requires s==S0;
  @ ensures s==S1(1);
  @*/
void f1(void) {
  g(1);
}
int main (void) {return 0;}
