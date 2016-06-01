/* run.config
   OPT: -wp-prop none -wp-msg-key refusage
*/

/* run.config_qualif
   DONTRUN:
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

/*@ axiomatic E {
  @ predicate Declared_equals_x{L}(int * q) reads *q,x ;
  @ predicate Defined_equals_x{L}(int * q) = *q == x ;
  @ }
  @ */

/*@ assigns \nothing;
  @ ensures (\result == 1) <==> Declared_equals_x(p); 
  @ */
int declared_equals_x (int *p);

/*@ assigns \nothing;
  @ ensures (\result == 1) <==> Defined_equals_x(p); 
  @ */
int defined_equals_x (int *p);

/*@ axiomatic R {
  @ logic integer f_rec(integer a) = (a < 100) ? f_rec(a+1+y) : x ;
  @ }
  @ */
//@ ensures \result == f_rec(b);
int recursive_usage(int b);
