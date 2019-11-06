/* run.config
   STDOPT: +"-wp-no-extensional"
*/
/* run.config_qualif
   COMMENT:
   STDOPT: +"-wp-no-extensional"
*/

/* -------------------------------------------------------------------------- */
/* --- GOAL: equality over records, arrays, pointers                      --- */
/* -------------------------------------------------------------------------- */

struct S { int a; int b;}; 
struct S s0,s1;

//@ ensures s0 == {{s1 \with .a = s0.a} \with .b = s0.b};  
void simple_struct(void){return;}

int t0[2];
int t1[2];

//@ ensures t0 == {{t1 \with [0] = t0[0]} \with [1] = t0[1]};
void simple_array(void){return;}

struct St {int tab[10];};
struct St st0,st1;

//@ ensures st0.tab == st1.tab ==> st0 == st1 ;
void with_array_struct(void){return;}

struct Sp {int * p ;};
struct Sp sp0,sp1;

//@ ensures sp0.p == sp1.p ==> sp0 == sp1;
void with_ptr_struct(void){return;}

int * tp0[5];
int * tp1[5];

//@ ensures (\forall integer i; 0 <= i < 5 ==> tp0[i] == tp1[i]) ==> tp0 == tp1;
void with_ptr_array(void){return;}

struct Q {int * qp ; int qt[2] ; struct S qs;};
struct Q q0,q1;

/*@
  ensures q0.qp == q1.qp ==> 
          q0.qs == q1.qs ==>
          q0.qt == q1.qt ==>
          q0 == q1 ;	     
*/ 
void with_ptr_and_array_struct(void){return;}
