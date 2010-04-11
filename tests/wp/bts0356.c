/*@ requires \valid_range(p,0,i);
    behavior default: ensures (\old(p)[i] == 78); */
void main(int *p , int i )
{
  p ++; p ++; p ++; *((p + i) - 3) = 78;
  return;
}
