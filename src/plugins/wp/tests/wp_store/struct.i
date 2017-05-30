/* run.config_qualif
   COMMENT:
*/


struct t {
  int tab[5];
};

/*@ requires \valid(i);
  @ ensures *i == 1;
  @ assigns *i ;
  @ */
void g(int* i) {
  *i = 1;
}

/*@ requires \valid(a) && \valid(a->tab+(0..4)); */
void f(struct t* a) {
  g (&a->tab[0]);
  //@ assert qed_ok: a->tab[0] == 1;
}

struct St {int a; int b;};
struct St v={1,2}, w={1,2} ; 
struct St * p = &v ;

/*@ ensures P: qed_ok: v == w;
  @ ensures Q: qed_ok: *p == w;
*/
void main(void) { return; }
