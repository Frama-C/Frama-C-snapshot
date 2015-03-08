/* run.config
   OPT: -wp-model +ref
*/

/* run.config_qualif
   OPT: -wp -wp-proof alt-ergo -wp-par 1 -wp-model +ref
*/

int t20[20] = {3} ;

/*@ ensures I0: t20[0]==3 ;
  @ ensures I1: t20[1]==0 ;
  @ ensures I2: t20[2]==0 ;
*/
void main (void) {return; } 
