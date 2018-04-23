int any_int(void);
int G[10] ={0};
int g=0,h=0,i=0,j=0,k=1,l=1,m=-1,n=-1;
void main () {
  int x;
  x = any_int();
  if (0 <= x)
    {
      g = x;
      }

  if (0 >= x)
    {
      h =x;
      }

  if (x >= 0)
    {
    i =x;
      }

  if (x <= 0)
    {
      j =x;
      }

  if (0 < x)
    {
      k =x;
      }

  if (0 > x)
    {
      m =x;
      }

  if (x > 0)
    {
    l =x;
      }

  if (x < 0)
    {
      n =x;
      }

  G[0] = 0;

  if ((0 <= x) && (5 >= x))
    G[0] = 7;

}
