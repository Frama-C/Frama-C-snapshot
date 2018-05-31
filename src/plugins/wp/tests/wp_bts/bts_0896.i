/* run.config_qualif
   DONTRUN: (config_qualif) useless
*/

/* run.config_qed
   DONTRUN: (config_qed) see config_qed
*/

// Checks that the axiom "ax{L}" is defined only once.

extern int tab[5], x;

//@ axiomatic A { axiom ax: 10 < \block_length(&tab[0]); }

//@ assigns x;
extern void h(void);

//@ requires r2: x==0; assigns x;
void g() { h(); }

//@ requires r1: x==0;
void f(void) { g(); }
