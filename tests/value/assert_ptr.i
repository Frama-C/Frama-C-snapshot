int *ptr, **q, s_q, a, r;

int main(int c, int d, int e)
{
  q = &s_q;
  if (c) ptr = &a;
  if (d) *q = (&a + e) ;
  /*@ assert ptr == 0 || ptr != 0 ; */
  Frama_C_show_each_ptr(ptr);
  if (ptr != 0) (*ptr)++;

  /*@ assert \valid(*q) && *q != 0 ; */
  Frama_C_show_each_q(s_q);

  return 0;
}
