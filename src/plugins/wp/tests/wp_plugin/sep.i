/* run.config
   OPT: -wp-extern-arrays -wp-model Caveat -wp-print-separation
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
