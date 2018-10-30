/* run.config 
   OPT: -wp-model Typed
*/

/* run.config_qualif
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="-qed_ko,-ko"
   OPT: -wp -wp-model Typed -wp-par 1 -wp-prop="qed_ko,ko" -wp-steps 50
*/

// Test for implicit variables

int x ;
int y ;
int *u ;
int *v ;

/*@ axiomatic A {
  @ predicate P(int *p,int *q)= *p <= *q ;
  @ predicate Q= x <= y ;
  @ }
  @ */

/*@ requires Q ;
  @ requires 0<=x<=10 ;
  @ requires 0<=y<=10 ;
  @ ensures qed_ok: Q ;
  @ */
void f(void) { x++; y++; }

/*@ requires P(u,v) ;
  @ requires 0<=*u<=10 ;
  @ requires 0<=*v<=10 ;
  @ ensures qed_ok: P(u,v) ;
  @ */
void g(void) { (*u)++; (*v)++; }

/*@ axiomatic S {
  @ predicate f{L} reads \nothing ;
  @ predicate g{L} reads x ;
  @ predicate h{L} reads x,y ;
  @ predicate w{L} ;
  @ }
  @*/

/*@ requires H: f && g && h && w ;
  @ ensures qed_ok: F_OK: f ; 
  @ ensures qed_ok: G_OK: g ;
  @ ensures qed_ko: H_KO: h ;
  @ ensures qed_ok: W_OK: todo: w ;
  @ */
void modifies_y ()
{
  y++ ;
}

/*@ requires H: f && g && h && w ; 
  @ ensures qed_ok: F_OK: f ; 
  @ ensures qed_ko: G_KO: g ;
  @ ensures qed_ko: H_KO: h ;
  @ ensures qed_ok: W_OK: todo: w ;
  @ */
void modifies_x ()
{
  x++ ;
}
