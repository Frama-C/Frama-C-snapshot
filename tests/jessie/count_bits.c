
/*@ axiomatic CountBits {
  @   // the i-th bit of x as 0 or 2^i
  @   logic integer bit(integer x, integer i);
  @   // the number of bits 1 in x
  @   logic integer nbits(integer x);
  @   axiom nbits_nonneg : \forall integer x; nbits(x) >= 0;
  @   axiom nbits_zero : nbits(0) == 0;
  @   // the lowest bit 1 in x
  @   logic integer lowest_bit(integer x);
  @   axiom lowest_bit_def :
  @     \forall integer x; x != 0 ==>
  @        \exists integer i; 
  @           i >= 0 &&
  @           lowest_bit(x) == bit(x,i) &&
  @           bit(x,i) != 0 &&
  @           \forall integer j; 0 <= j < i ==> bit(x,j) == 0;
  @   axiom lowest_bit_zero :
  @     \forall integer x; lowest_bit(x) == 0 <==> x == 0;
  @   axiom lowest_bit_trick :
  @     \forall integer x; (x & -x) == lowest_bit(x);
  @   axiom remove_one_bit :
  @      \forall integer x, integer i;
  @         bit(x,i) != 0 ==> nbits((int)(x - bit(x,i))) == nbits(x) - 1;
  @ }
  @*/

/*@ ensures \result == nbits(x); */
int count_bits(int x) {
  int d, c;
  /*@ loop invariant c + nbits(x) == nbits(\at(x,Pre));
    @ loop variant   nbits(x);
    @*/
  for (c = 0; d = x&-x; x -= d) c++;
  return c;
}


/* 
Local Variables:
compile-command: "LC_ALL=C make count_bits"
End:
*/
