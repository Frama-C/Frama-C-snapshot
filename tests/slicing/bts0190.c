/*  run.config
OPT: -check -slice-rd y
*/
int z1(void);

int x(int y, int z){
/*@ slice pragma expr y == 1; */
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
 return 1;
}
