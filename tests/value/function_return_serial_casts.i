short x = -10;
int y, z, t;

unsigned short f(void)
{
  return x;
}

unsigned short g(void)
{
  unsigned short l = *(unsigned short*)&x;
  return l;
}

main(){
  y = *(unsigned short*)&x;
  z = f();
  t = g();
} 
