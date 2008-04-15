
// we do not check for arithmetic overflows
#pragma IntModel(exact)

// lemmas to help automatic provers 
/*@ lemma dist1: \forall integer x,y,z; x*(y+z) == x*y + x*z; 
  @ lemma dist2: \forall integer x,y,z; (x+y)*z == x*z + y*z; 
  @ lemma id1: \forall integer x; x*1 == x; 
  @ lemma id2: \forall integer x; 1*x == x; 
  @*/

int i;
int j;

/*@ ensures i == \old(j) + k;
  @ ensures j == 3 * \old(j) + 11 * k + 12; 
  @*/
void test(int k)
{
  int l = 1;
  int m = 12;
  i = j + k;
  l *= j ;
  j += l + 10 * k + i + m;
}


/* 
Local Variables:
compile-command: "PPCHOME=../.. LC_ALL=C make arith"
End:
*/
