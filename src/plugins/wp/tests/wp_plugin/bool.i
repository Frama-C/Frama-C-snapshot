/* run.config
   OPT: -wp-no-let -wp-no-bool-range
   OPT: -wp-no-let -wp-bool-range 
*/

/* run.config_qualif
   OPT: -wp-no-let -wp-no-bool-range
   OPT: -wp-no-let -wp-bool-range
*/


/*@ ensures 0 <= \result <= 2 ; */
int job(_Bool a, _Bool b) { return a+b; }
