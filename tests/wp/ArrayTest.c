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

int x[2][3][5] = 
  {
    {{ 111,112,113,114,115 },
     { 121,122,123,124,125 },
     { 131,132,133,134,135 }} ,
    {{ 211,212,213,214,215 },
     { 221,222,223,224,225 },
     { 231,232,233,234,235 }}
  };

int *p1;
int *p2; 
int *p3;

int x1;
int x2;
int x3;

/*@ 
  ensures x1 == 211 ;
  ensures x2 == 212 ;
  ensures x3 == 123 ;
*/
void main () {

  p1 = (int *)(x+1);
  x1 = (*p1);
  
  p2 = p1+1;
  x2 = *p2;
  
  p3 = x[0][1]+2;
  x3 = *p3;
  
} 
