/* run.config
  GCC:
  OPT: -val -deps -out -input  -main main0 -journal-disable
  OPT: -val -deps -out -input  -main main1 -journal-disable
  OPT: -val -deps -out -input  -main main2 -journal-disable
  OPT: -val -deps -out -input  -main main3 -journal-disable
  OPT: -val -deps -out -input  -main main4 -journal-disable
  OPT: -val -deps -out -input  -main main4bis -journal-disable
  OPT: -val -deps -out -input  -main main5 -journal-disable
  OPT: -val -deps -out -input  -main main5bis -journal-disable
*/

int k,i,j,x,c,d,T[10];

void main0(){

  if (j)
    {if (c) x=i; else x=d;}
  else x=k;
}

void main1(){

  if (j)
    {if (c) T[0]=i; else T[1]=d;}
  else x=k;
}

void main2(){
  if (j)
    {if (c) ((int*)((char*)T+1))[0]=i; else T[1]=d;}
  else x=k;
}


void main3(){
  int* p = ((int*)((char*)T+1));

  if (c) { p[0]=i;  p[1]=d;}  else T[1] = x;

}

void main4()
{
  if (c)
    {
      T[0]=i;
      T[2]=j;
    }
  else
    {
      T[0]=k;
    }

}

void main4bis()
{
  if (c)
    {
      T[0]=k;
    }
  else
    {
      T[0]=i;
      T[2]=j;
    }
}

void main5()
{
  if (c)
    {
      T[0]=i;
      T[1]=j;
    }
  else
    {
      T[0]=k;
    }

}

void main5bis()
{
  if (c)
    {
      T[0]=k;
    }
  else
    {
      T[0]=i;
      T[1]=j;
    }
}
