/*@
  requires *s >= 0 ;
*/
void dummy(int *s);

void foo(void)
{
  int p[1] = { 0 };
  dummy(p);
}
