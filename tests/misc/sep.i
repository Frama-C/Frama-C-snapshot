/* run.config
   OPT: -val -slevel 10 -lib-entry -main f1 -separate-n 0 -separate-of 3
   OPT: -val -slevel 10 -lib-entry -main f1 -separate-n 1 -separate-of 3 
   OPT: -val -slevel 10 -lib-entry -main f1 -separate-n 2 -separate-of 3 
   OPT: -val -slevel 10 -lib-entry -main f1 -separate-n 3 -separate-of 3 
   OPT: -val -slevel 10 -lib-entry -main f1 -val-split-return-auto
*/
int index;
int tab[5];

//@ ensures \result==0 || \result==-1 || \result==1 ;
extern int init2(void);

int init1(void)
{
	int res;

	res = init2();

	if (res == 0)
	{
		index=0;
	}
	else
	  {
	    if (res == 1)
	      {
		res = 0;
		index = 0;
	      }
	  }

	return res;
}

//@ requires 0<=n<5;
int f1(int n)
{
	int res;

	res = init1();

	if (res == 0)
		return tab[index+n];
        return -1;
}
