/* run.config
   OPT: -wp-model Typed
   OPT: -wp-model Hoare
*/

/* run.config_qualif
   OPT: -wp -wp-par 1 -wp-model Typed
*/

/* run.config_qed
   DONTRUN: (config_qed)
*/

/* -------------------------------------------------------------------------- */
/* --- GOAL: complete initialization of value into or out memory          --- */
/* -------------------------------------------------------------------------- */


struct St {int a; int b;};
struct St v={1,2}, w={1,2} ; 
struct St * p = &v ;

/*@ ensures P: v == w;
  @ ensures Q: *p == w;
*/
void main(void) { return; }
