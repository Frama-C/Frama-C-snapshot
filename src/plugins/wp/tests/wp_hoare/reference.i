/* run.config
   OPT: -wp-model +ref
*/

/* run.config_qualif
   OPT: -wp-model +ref
*/

/*@
   requires \valid(p);
   assigns \nothing ; 
   ensures \result == *p ;
 */ 
int f(int * p);

/*@ 
  assigns \nothing ; 
  ensures \result == 4 ; 
 */ 
int g (int x){
  x = 4 ; 
  return f(&x);
}


/*@ requires \valid(p2) && \valid(q) ; 
    assigns \nothing ; 
    ensures \result == *p2+*q;
*/
int f2(int *p2, int * q);

/*@
    requires \valid(ptr); 
    assigns \nothing;
    ensures \result == 4;
 */
int call_f2(int * ptr, int y)
{
  y = 2 ; *ptr =2; 
  return f2(ptr,&y);
}


int *gl; 

/*@ requires \valid(gl) ; assigns \nothing; ensures \result == *gl;
 */
int call_global (void)
{ return f(gl);}


/*--------------------------------------------*/



/*@ requires \valid(pa);
    assigns *pa; 
    ensures *pa == kb;
 */
void write (int kb,int * pa)
{ *pa = kb;}
