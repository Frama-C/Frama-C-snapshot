/* run.config
   OPT: -wp-model +ref -wp-no-let
   OPT: -wp-model +ref -wp-let
*/

/* run.config_qualif
   OPT: -wp-model +ref -wp-no-let
   OPT: -wp-model +ref -wp-let
*/

/*@ 
    requires \valid(rp);
    assigns *rp;
    ensures *rp == 0;
*/
void reset (int *rp) {*rp = 0;}

/*@ 
    requires \valid(ip);
    assigns *ip;
    ensures *ip == \old(*ip)+1;
*/
void incr (int *ip) {*ip=*ip+1;}


/*@ 
    requires \valid(lp);
    assigns \nothing;
    ensures \result == *lp ;
*/
int load (int *lp) {return *lp;} 

int x; 

/*@
    assigns x; 
    ensures \result == 0;
 */
int call_global (void) 
{
 reset(&x); 
 return (load(&x));
}

/*@ requires \valid(&y);
    assigns \nothing; 
    ensures \result == 0;
 */
int call_param (int y) 
{
 reset(&y); 
 return (load(&y));
}

/*@ assigns \nothing; 
    ensures \result == 0;
 */
int call_local(void) 
{
 int z;
 reset(&z); 
 return (load(&z));
}

/*@ 
    requires \valid(q);
    assigns *q; 
    ensures \result == 0;
 */
int call_param_ref (int *q)
{
 reset(q); 
 return (load(q));
}
