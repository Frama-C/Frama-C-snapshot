/*@ 
  ensures A: \result == (-129 & x) ; 
  ensures B: \result == (0x7F & x) ;
*/
int compute(unsigned char x)
{
  return x & 0xFF7F ;
}
