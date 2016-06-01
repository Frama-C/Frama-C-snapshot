/*@ logic integer Id(integer k) = k ; */

int s ;

/*@ ensures s == \old( 0 <= x <= 100 ? Id(s+x) : Id(s) ); */
void job(int x)
{
  if (0 <= x && x <= 100) s+=x;
}
