int t[20]={1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0};

int main(void)
{
  int i,j=0,X=0;
  for (i=0;i<8;i++)
    j=i;
  if (j<=7) X=j;
  X=t[X];
  return X;
}
  
