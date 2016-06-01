long long T[10000];
int U[10000];
void main () {
  int i,j;
  for (i=0;i<5; i++)
    T[i] = 2;

  for (j=6;j<10000; j++)
    T[j] = 7;

  i=0;
  while(1)
    {
      U[i]=0;
      if (i == 200) U[i]=-1;
      i = 1000 - i;
      if (i < 500)
	i++;
      if (i == 400)
	goto l_end_loop;
    }
 l_end_loop:
}
