/* run.config
   OPT:  -journal-disable -wp -wp-model Store -wp-proof none
   OPT:  -journal-disable -wp -wp-model Hoare -wp-proof none
*/ 

/* run.config_ergo
   OPT:  -journal-disable -wp -wp-model Store -wp-proof alt-ergo
   OPT:  -journal-disable -wp -wp-model Hoare -wp-proof alt-ergo
*/

/* run.config_z3
   OPT:  -journal-disable -wp -wp-model Store -wp-proof z3
   OPT:  -journal-disable -wp -wp-model Hoare -wp-proof z3
*/

/* run.config_simplify
   OPT:  -journal-disable -wp -wp-model Store -wp-proof simplify
   OPT:  -journal-disable -wp -wp-model Hoare -wp-proof simplify
*/


/* 
   kind : Positive
   model name : Store ; bhv : Proved with alt_ergo
   model name : Hoare ; bhv : Proved with all
 */

//@ assigns \nothing;
int assigns_nothing(void) 
{ return 5;
}

int r,g;

// two tset
//@ assigns r,g ;
void assigns_two_loc(void) 
{ r=5; g=8;}

//@ assigns \union(r,g) ;
void assigns_union_of_two_loc(void) 
{ r=1; g=2;}

//@ assigns r; 
void assigns_no_cst(void) 
{ r=g;}


/*@
  behavior NULL : assumes i==0; assigns r;
  behavior OTHER : assumes i!=0;assigns \nothing;
 */ 
void assigns_cond(int i)
{
  if (i==0) r=12 ; 
}

//@ requires i>0; ensures i>0;
void no_assigns (int i)
{
  if (i==0) r=12 ; 
}

int main (void) {return 0;}


