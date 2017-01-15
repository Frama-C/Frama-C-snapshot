
extern const int endian[];

#define BIT(m,k) (((m) & (1 << (k)-1 )) != 0)

/*@
  behavior OutRange:
  assumes m < 0 || m > 255 ;
  ensures \result == -1 ;

  behavior InRange:
  assumes 0 <= m <= 255 ;
  ensures \forall integer k; 1 <= k <= 8 ==> (BIT(\result,k) == BIT(m,9-k));
*/

int reverse2(int m)
{
  if (0 <= m && m <= 255)
    return endian[m];
  else
    return -1;
}
