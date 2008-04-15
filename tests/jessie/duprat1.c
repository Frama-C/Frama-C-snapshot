/*@ logic boolean cond1(integer p) = (p>0)?\true:\false ;
 @ logic boolean cond2(integer p) = (p<10)?\true:\false ;
 @ logic boolean cond3(boolean c1, boolean c2) = c1 && c2 ;
 @*/

/*@
 @ ensures (cond1(x) && cond2(y)) ==> \result == 1 ;
 @ ensures \result == ((cond3(cond1(x),cond2(y)))?1:0);
 @*/
int ftest(int x, int y)
{
 return (x>0 && y<10);
}

/* 
Local Variables:
compile-command: "LC_ALL=C make duprat1"
End:
*/
