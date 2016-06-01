
int x = 0;
int y = 0;
int z = 0;
int *pz = &z ;
int *px = &x ;
int *py = &y ;

int X = 0;
int *pX;

struct s 
{ int ok;
  int **p; } t[5]={ {0,0}, {1,&pz},  {1,&py} ,  {0,0} };

struct s t2[5]={ {0,0}, {0,0}, {1,&px}, {0,0} };
int ii[2];

void main(void)
{
  int i; volatile int k=0;
  pX = k ? 0 : &X;
  for (i=0 ; i < 5; i++)
    {
      if (t[i].ok)
        **(t[i].p) = i;
      X = i;
      }
  
  for (ii[1]=0 ; ii[1] < 5; ii[1]++)
    {
      if (t2[ii[1]].ok)
        **(t2[ii[1]].p) = ii[1];
      X = ii[1];
      }
}
  
void g (void)
{
  int c = -25;
  

  while (c)
    {
      c++;
    }
}
