int y,t;
int R1,R2,R3,R4;
int c;
int tab[9] = { 101, 102, 103, 104, 105, 106, 103, 102, 101 };

void main(int x)
{
  y = x;
  t = y + 10;
  if (x == 2)
    {
      R1 = y;
      R2 = t;
    }

  if (t == 17)
    R3 = x;

  if (x>=0 && x<=5)
    if (tab[y] == 103)
      R4 = x;
}
