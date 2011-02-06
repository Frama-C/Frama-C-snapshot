int i;

//@ requires 1100000000<=i<=1100000001;
int f()
{
  //@ assert 1100000000<=i<=1100000001;
  return i;
}
