typedef enum {

 OK = 0,
 NOT_OK = 1
} RESULTAT;


void p1(RESULTAT *last, int n);

RESULTAT f2(int n);

/*@ predicate result{L}(RESULTAT status, int n) =  
  @ (status == OK) && (n>0) ;
  @*/

/*@ predicate function2_res{L}(int n) =
  @  result((RESULTAT)OK,n);
  @*/


/*@ requires \valid(last);
  @ ensures result{Here}(\old(*last),n) ==> *last == OK;
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

return *initial ;

}

/* 
Local Variables:
compile-command: "LC_ALL=C make geanta2"
End:
*/
