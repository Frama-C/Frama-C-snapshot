/*@ requires \valid(a);
  @ behavior anz:
  @   assumes *a != 0;
  @   ensures *a == 0;
  @ */
void f(int *a)
{
  if (*a != 0)
    *a = 0;
}

/*
Local Variables:
compile-command: "LC_ALL=C make -j assume"
End:
*/
