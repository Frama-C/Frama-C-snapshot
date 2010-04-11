/*  run.config
OPT: -slice-return x  -journal-disable -slice-print
*/
/*@ requires y == 1;
  @ ensures
  @ \result == 2*y;
  @*/
int x(int y, int z)
{
//@ assert y == 1;
//@ assert y + z == 3;
 return 2*y*z1();
}

int main()
{
 x(1,2);
 return 0;
}

int z1()
{
 return x(2,2);
}
