typedef enum {

 OK = 0,
 NOT_OK = 1
} RESULTAT;


void p1(RESULTAT *last, int n);

RESULTAT f2(int n);

/*@ predicate result{L}(RESULTAT status, int n) =  
  @ (status == OK) && (n>0) ;
  @*/

/*@ axiomatic statusInitial {
  @ logic RESULTAT status_initial{L}(integer i);
  @ 
  @ axiom status_initial_ok{L} :
  @ \forall integer i;
  @ status_initial{L}(i) == OK;
  @ 
  @ predicate function2_res{L}(int n) =
  @  result(status_initial{L}(1),n);
  @ }
  @*/

/*@ requires \valid(last);
  @ assigns *last;
  @ ensures result{Here}(\old(*last),n) ==> (*last) == OK;
  @*/
void p1(RESULTAT *last, int n) 
{

if (*last == OK) {

if(n>0) 
*last= OK;
else 
*last= NOT_OK;

}

}

/*@ ensures function2_res{Here}(n) ==> \result == OK;
  @*/
RESULTAT f2(int n) 
{

RESULTAT *initial ;

*initial = OK;

p1(initial,n);

return (*initial) ;

}

/* 
Local Variables:
compile-command: "LC_ALL=C make geanta3"
End:
*/
