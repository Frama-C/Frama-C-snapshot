/* run.config
   OPT: -wp-extern-arrays -wp-model Caveat
 */

/* run.config_qualif
   DONTRUN: No Proofs
*/

int a ;
int b ;

//HYP: none 
//@ ensures a == \old(a) + k;
void f1_none(int k) { a += k ; }

//HYP: \separated( p , &a ) 
//@ ensures a == \old(a) + *p;
void f2_p_a(int *p) { a += *p; }

//HYP: \separated( p , {&a,&b} ) 
//@ ensures a == b + *p; 
void f3_p_ab(int *p) { a = b + *p; }

//HYP: \separated( p , q , {&a,&b} ) 
//@ ensures a == \old(a) + *p && b == \old(b) + *q ; 
void f4_pq_ab(int *p,int *q) { a += *p; b += *q; }

//HYP: \separated( p , q ) 
//@ ensures *p == \old(*p) + *q; 
void f5_pq(int *p,int *q) { *p += *q; }

//HYP: \separated( p+(..) , &a ) 
//@ ensures p[k] == a ; 
void f6_Pa(int *p,int k) { p[k] = a; }

static int c ;
static int d ;

int * escape_addrof_d(void) { return &d; }

//HYP: \separated( p , q , {&a,&d} ) because of static
//@ ensures a == \old(a) + *q ;
void f7_pq_ad(int *p,int *q) { c += *p; d += *q ; a += *q ; }

//@ ghost int g ;

//HYP: \separated( p , q , &a ) because of ghost
//@ ensures a ==\old(a) + *q ; 
void f8_pq_a(int *p,int *q) { /*@ghost g += *p; */ a += *q ; }
