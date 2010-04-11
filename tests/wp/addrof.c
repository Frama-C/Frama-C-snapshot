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
   model name : Store ; bhv : Proved with alt_ergo
   model name : Hoare ; bhv : Out of Scope
 */
int A;

/*@ ensures A == 5 ; */
int main() {
  int *p = &A;
  *p = 5; 
  return *p;
}
