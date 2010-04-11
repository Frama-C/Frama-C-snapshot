/* run.config
   OPT:  -journal-disable -wp -wp-model Store -wp-proof none
   OPT:  -journal-disable -wp -wp-model Hoare -wp-proof none
*/ 

/* run.config_ergo
   OPT:  -journal-disable -wp -wp-model Store -wp-proof alt-ergo
*/

/* run.config_z3
   OPT:  -journal-disable -wp -wp-model Store -wp-proof z3
*/

/* run.config_simplify
   OPT:  -journal-disable -wp -wp-model Store -wp-proof simplify
*/


/* 
   kind : Positive
   model name : Store ; bhv : Provable
   model name : Hoare ; bhv : Out of Scope
 */
/*@ ensures *x == 3;
    assigns *x;
 */
void ptr_deref_assigns (int * x) {
  *x = 3;
}


int * p; 
int a; 

//@ assigns p,*p; 
void ptr_deref_ptr_assigns (void)
{
  p = &a; 
  *p = 5;
}

//@ assigns p , *\old(p);
void deref_ptr_ptr_assigns (void)
{
  *p=5;
  p=&a;
}

int x,y;
struct S { int * a; };

int main () { return 0;}

