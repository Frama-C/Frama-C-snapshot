/* run.config 
   DONTRUN: test under construction
*/

typedef float Numeric ;
/*@ assigns \nothing ;
  @*/
Numeric f (Numeric num1,Numeric num1) ;

typedef const struct { Numeric i; Numeric j;} Numeric2;
void Type3Ex3 (int e1, Numeric *p1, Numeric2 t1[], Numeric t2[]) {
  
  int s ;  
  while ((s < (sizeof (t1) / sizeof (Numeric2)) - 2) &&
         (e1 >= t1[s + 1].i))
    //@ assert \false;
    s = s + 1 ;
  *p1 = f (e1 - t1[s].i, t2[s]) + t1[s].j ;
}
