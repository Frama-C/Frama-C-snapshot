/* run.config
   OPT: -check -calldeps -slice-return send1 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -calldeps -slice-return send2 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -calldeps -slice-return send3 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -calldeps -slice-return send4 -journal-disable -then-on 'Slicing export' -print
   OPT: -check -calldeps -slice-return send1 -slice-return send4 -journal-disable -then-on 'Slicing export' -check -calldeps -slice-return send1_slice_1 -print  -then-on 'Slicing export 2' -print







   */

/* Small example devired from examples given for UNRAVEL tool : */



/*@ assigns *p \from \empty;
    assigns \result ; */
int scanf (char const *, int * p);



int printf (char const *, int);








int send1 (int x) {
  printf ("%d\n", x) ;
  return x;
}
int send2 (int x) {
  printf ("%d\n", x) ;
  return x;
}
int send3 (int x) {
  printf ("%d\n", x) ;
  return x;
}
int send4 (int x) {
  printf ("%d\n", x) ;
  return x;
}

main()
{
	int		input1,input2,input3,cond1,cond2;
	int		a,b,c;
	int		*x,*y,*z;
	int		output1,output2,output3;

	scanf("%d",&input1);
	a = input1;
	scanf("%d",&input2);
	b = input2;
	scanf("%d",&input3);
	c = input3;
	scanf("%d",&cond1);
	scanf("%d",&cond2);
	x = &a;
	if (cond1) x = &b;
	y = &c;
	z = &b;
	output2 = *z + 1;
	*z = *y + *x;
	output1 = *z;
	output3 = *x;
	send1 (output1);
	send2 (output2);
	send3 (output3);
	send4 (cond2);
}
