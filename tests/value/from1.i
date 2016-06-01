int G;

int cx,cy,cz,sx,sy,s;

struct Tstr { int a; int b; }; 


void f(void)
{
  cy = cx;
}

int sf (struct Tstr * ps) {
  return ps->a;
}

int main(int x,int y) {
  struct Tstr s = {sx, sy};

  if (x) G=y;

  cx = cz;
  f();
  
  return sf(&s);
}


