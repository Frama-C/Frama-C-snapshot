/* run.config*
   OPT: -val @VALUECONFIG@ -warn-signed-overflow
*/

#include <limits.h>

int cl, cu, ucu;
int il, iu, uiu;
long ll, lu;
unsigned long ulu;
long long lll, llu;
unsigned long long ullu;

main()
{
  cl = CHAR_MIN;
  cu = CHAR_MAX;
  ucu = UCHAR_MAX;

  il = INT_MIN;
  iu = INT_MAX;
  uiu = UINT_MAX;

  ll = LONG_MIN;
  lu = LONG_MAX;
  ulu = ULONG_MAX;

  lll = LLONG_MIN;
  llu = LLONG_MAX;
  ullu = ULLONG_MAX;
}
