/*@ axiomatic BuchTransStart {
  @   logic integer buch_Trans_Start(integer tr);
  @   axiom buch_Trans_Start0: buch_Trans_Start(0) == 0;
  @ }
  @*/

/*@
  predicate I_3 =
    \forall integer _buch_st;
      (0 <= _buch_st && _buch_st < 6)
       ==>
         \exists integer _buch_tr;
	   0 <= _buch_tr && 
	   _buch_tr < 7 &&
	   buch_Trans_Start(_buch_tr) == _buch_st ;
 */


//@ ensures I_3;
int main(void) 
{ int __retres ;
  
  __retres = 1;
  
  return (__retres);
}
