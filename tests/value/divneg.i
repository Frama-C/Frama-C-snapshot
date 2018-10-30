extern int i;

int r0, r1 = 6, r2 = -8, r6, r7, r8, r9, rn1 = 5, rn2, rn3, rn4=-3, rn5;

int vic;

int main (int c)
{
  int rc;
  if (c<0) c=0;
  if (c>20) c=0;
  c = - 5 - 9 * c;
  Frama_C_show_each(c, c / (-3)); 
  if (i >= -100 && i <= 100)
  {
    if (i % 5 == 0)
      r0 = i;
    if (i % 5 == 1)
      r1 = i;
    if (i % 5 == -3)
      r2 = i;
    if (i % 5 == 6)
      r6 = 1;
    if (!(i % 5 == 7))
      r7 = i;
    if (i % 5 != 8)
      r8 = i;
    if (!(i % 5 != 9))
      r9 = 1;
    if (i % 2 != 0)
      rn1 = i;
    if (!(i % 2 != 0))
      rn2 = i;
    if (i % 2 == 0)
      rn3 = i;
    if (!(i % 2 == 0))
      rn4 = i;
    if (i % 3 != 1)
      rn5 = i;
    if (c % 18 == -14)
      rc = c;
    Frama_C_dump_each();
  }
  rc = 104;
  c = -c;
  if (c % 18 == 14)
    rc = c;


  *(unsigned int *)&vic = -1U;
  if (vic % 5 == -1)
    Frama_C_show_each_should(vic);
  else
    Frama_C_show_each_shouldnt(vic);
  return 0;
}
