
extern const int endian[];

#define BIT(m,k) (((m) & (1 << (k)-1 )) != 0)

/*@
  behavior OutRange:
  assumes m < 0 || m > 255 ;
  ensures \result == -1 ;

  behavior InRange:
  assumes 0 <= m <= 255 ;
  ensures    (BIT(\result,1) == BIT(m,8))
          && (BIT(\result,2) == BIT(m,7))
          && (BIT(\result,3) == BIT(m,6))
          && (BIT(\result,4) == BIT(m,5))
          && (BIT(\result,5) == BIT(m,4))
          && (BIT(\result,6) == BIT(m,3))
          && (BIT(\result,7) == BIT(m,2))
          && (BIT(\result,8) == BIT(m,1)) ;
*/

int reverse(int m)
{
  if (0 <= m && m <= 255)
    return endian[m];
  else
    return -1;
}
