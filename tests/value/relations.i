
int t[3]={1,2,3};
int *p,x;
int u[20];
int R1,R2,R3,R4,R5,R6,R7,A7,R8,A8;
int S1,S2,S3,S4,S5,S6,S7,B7,S8,B8;

typedef struct {
int L0;
int L1;
int T13;
int T;
int L8;
} Cs;

void main(int c,char d,char e, int f, int g, int h, int i, Cs *pCs)
{ 
  u[0] = g;
  p=&t[1];
  *p=4;
  if (c) c=0;
  t[0]=t[1];
  x=*(p-1);
  e=d;
  e=d-e +1;
  if (d) (*(char*)&f)=e; else f = x;

  u[1] = u[0];
  if (u[1] == 3)
    {
      R1 = u[0];
      R2 = g;
    }
  
  u[5] = u[0] + 1;
  if (u[5] == 3)
    {
      R3 = u[0];
      R4 = g;
    }
  R5 = u[5] - u[0];
  
  u[10] = h;
  u[11] = i;
  if (u[10] == u[11])
    R6 = u[10] - u[11];

  A7 = u[1] - u[0];
  if (u[1] == u[0])
    R7 = 1;

  A8 = u[5] - u[1];
  if (u[5] == u[1])
    R8 = 1;

  pCs->T13 = pCs->L0 || pCs->L1;
  pCs->T = pCs->T13;
  pCs->L8 = pCs->L0 || pCs->T13;

  S1 = pCs->T - pCs->T13;
  if ( pCs->T == pCs->T13)
    S2 = 1;
}
