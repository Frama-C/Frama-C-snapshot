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
   model name : Store ; bhv : Not Sure About
   model name : Hoare ; bhv : Not Sure About 
 */

int r;

//@ requires i==1; ensures r==3;
void no_loop_assigns (int i)
{ 
  while(i!=4) {r=i ; i++;}
}

//@ requires i==1; ensures r==3; 
void loop_with_assigns (int i)
{ 
  //@ loop assigns r,i;
  while(i!=4) {r=i ; i++;}
}

int main (void) {return 0;}
