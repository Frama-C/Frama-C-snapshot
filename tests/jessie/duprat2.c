/*@ predicate pcond1(integer p) = (p>0)?\true:\false ;
 @ predicate pcond2(integer p) = (p<10)?\true:\false ;
 @ predicate pcond3(boolean c1, boolean c2) = c1 && c2 ;
 @*/

/*@
 @ ensures (pcond1(x) && pcond2(y)) ==> \result == 1 ;
 @ ensures \result == 1 <==> pcond1(x) && pcond2(y);
 @*/
int ftest2(int x, int y)
{
 return (x>0 && y<10);
}

/* 
Local Variables:
compile-command: "LC_ALL=C make duprat2"
End:
*/
