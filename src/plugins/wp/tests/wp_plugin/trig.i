/* run.config_qualif
   OPT: -wp -wp-par 1
*/

typedef struct MSG {
  int size ;
  unsigned char text [65536] ;
} message ;

/*@ axiomatic Messages {
  @ logic message concat( message a , message b );
  @ 
  @ axiom cats: \forall message a,b ; 
  @   concat(a,b).size == a.size + b.size ;
  @   
  @ axiom cat1: \forall message a,b,c ; \forall integer k ;
  @   c == concat(a,b) ==>
  @   0 <= k < a.size ==> 
  @   c.text[k] == a.text[k] ;
  @   
  @ axiom cat2: \forall message a,b,c ; \forall integer k ;
  @   (TRIGGER: c == concat(a,b)) ==>
  @   a.size <= k < a.size + b.size ==> 
  @   (TRIGGER: c.text[k]) == b.text[k - a.size] ;
  @   
  @ }
  @ */

/*@ requires qed_ok: a.size >= 0 && b.size >= 0 ;
  @ ensures \result == concat(a,b) ;
  @ assigns \nothing ;
  @*/
message fconcat(message a,message b);

/*@ requires a.size == 5 && b.size == 5 ; */
void foo(message a,message b)
{
  message c = fconcat(a,b);
  //@ assert qed_ok: S: c.size == 10 ;
  //@ assert qed_ok: A: c.text[2] == a.text[2] ;
  //@ assert qed_ok: B: c.text[7] == b.text[2] ;
}
