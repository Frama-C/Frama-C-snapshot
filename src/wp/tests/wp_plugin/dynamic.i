/* run.config
   OPT: -wp-dynamic
*/

/* run.config_qualif
   OPT: -wp-dynamic -wp
*/

/*@ 
  requires -10<=x<=10;
  ensures \result == x+1; 
  assigns \nothing; 
*/
int f1(int x);

/*@ ensures \result == x-1; assigns \nothing; */
int f2(int x);

typedef struct S {
  int param ;
  int (*f)(int) ;
} ;

/*@
  requires (closure->f == &f1 && \abs(closure->param)<=5) || closure->f == &f2 ; 
  ensures \abs(\result - closure->param) <= 1 ;
 */
int call(struct S * closure) {
  /*@ calls f1,f2 ; */
  return (closure -> f)(closure -> param) ;
}
