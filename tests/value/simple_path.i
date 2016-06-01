int G,H;
int *p = &G;
int *q = &H;

void main(void)
{
  G = 4;
  *p = 3;
  p = &H;
  *p = 5;
}
