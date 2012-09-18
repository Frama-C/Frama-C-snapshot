int *p;

main(){
	int i;
	for (i=0; i<2; i++)
	{
		int a;
		if (i==0)
		{
			Frama_C_split(0);
			p=&a;
			Frama_C_merge(0);
		}
		*p = 3;
	}
}
