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

int i,b;

//@ ensures  b == 0;
int res_post_incr(void)
{
  i =0;
  b = i++; 
  return i;  
}

//@ ensures  b == 0;
int res_post_decr(void)
{
  i =0;
  b = i--; 
  return i;  
}

//@ ensures  b == 1;
int res_pre_incr(void)
{
  i =0;
  b = ++i; 
  return i;  
}

//@ ensures  b == 0;
int res_pre_decr(void)
{
  i =1;
  b = --i; 
  return i;  
}

//@ ensures  b == \old(i);
int res_post_incr_old(void)
{

  b = i++; 
  return i;  
}

//@ ensures  b == \old(i);
int res_post_decr_old(void)
{
  b = i--; 
  return i;  
}

//@ ensures  b == i;
int res_pre_incr_old(void)
{
  b = ++i; 
  return i;  
}

//@ ensures  b == i;
int res_pre_decr_old(void)
{
  b = --i; 
  return i;  
}
int main (void) { return 0 ; }
