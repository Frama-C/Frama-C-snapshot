/* run.config_phoare
  OPT:  -journal-disable -wp -wp-model Hoare -wp-proof alt-ergo -wp-verbose 2
*/
/* run.config_pruntime
   OPT: -wp -wp-model Runtime -wp-no-logicvar -journal-disable -wp-verbose 2 -wp-proof simplify
*/



//@ logic int * Shift_Ptr(int * p, integer i) = p + i ;

//@ ensures bug:\result == Shift_Ptr(p, i);
int * p_shift (int * p, int i) {
  return p + i;
}


typedef int Array[12] ;
struct st { Array t ; int a ;} s1 , s2;

//@ logic Array Mk_Array (Array x) = x ;

/*@ ensures bug1: s1 == { s1 \with .t = Mk_Array(s2.t) } ;
    ensures ok1:  s1 == { s1 \with .t = s2.t } ;

    ensures bug2: \let v = \old(s1.a) ; 
                  s1 == { s2 \with .a = v } ;
    ensures ok2:  s1 == { s2 \with .a = \old(s1.a) } ;
*/
void wr_struct (int x) {
  int a = s1.a ;
  s1 = s2 ;
  s1.a = a ;
}
