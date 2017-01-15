/*@
  ensures \result == ((m&2) ? -a : a ) + ((m&1) ? -b : b );
*/
int op(int m,int a,int b)
{
  switch( m & 3 ) {
  case 0: return a+b;
  case 1: return a-b;
  case 2: return b-a;
  case 3: return -a-b;
  }
}
