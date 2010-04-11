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
   model name : Store ; bhv : Provable
   model name : Hoare ; bhv : Proved with all
 */


//@ ensures \result == 0;
int and_op(void) 
{
  return (1==5 && 2 == 2);
}

//@ ensures \result == 1;
int or_op(void) 
{
  return (1==5 || 2 == 2);
}


/*@ 
 behavior Case_True : 
    ensures x==5 ==> \result == 1;
 behavior Case_False : 
    ensures x!=5 ==> \result ==0;
*/
int cond_op(int x)
{
  
  return (x==5?1:0);
}


int blue; 
int green;
int yellow;

/*@

  behavior Case_2 : 
  ensures x==-2 ==> \result == blue ;
   
   behavior Case_3:
   ensures x==-1 ==> \result == yellow;
   
   behavior default :
   ensures (x >-1|| x <-2 )==> \result == green;
 */

int other_color(int x)
{
  int r = x+2 ; 
  if (r==1) return yellow; else
    if(r==0) return blue ; else return green; 
}

int main (void) {return 0;}
