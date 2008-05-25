
int t[3]={1,2,3};
int *p,x;

void main(int c,char d,char e, int f)
{ 
  p=&t[1];
  *p=4;
  if (c) c=0;
  t[0]=t[1];
  x=*(p-1);
  e=d;
  e=d-e +1;
  if (d) (*(char*)&f)=e; else f = x;
  return;
}
