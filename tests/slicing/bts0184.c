/* run.config
   OPT: -check -slice-pragma x  -journal-disable
 **/
int x(int y, int z)
{
/*@ slice pragma expr y == 1; */
//@ assert y == 1;
//@ assert y + z == 3;
 return y;
}

int main()
{
 return 0;
}

int z1()
{
 return x(2,2);
}
