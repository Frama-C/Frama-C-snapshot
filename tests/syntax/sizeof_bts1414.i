int f(int b);
int g(int *a)
{
  int x ;
  x = sizeof(f(*a));
  switch (x) {
    case (sizeof(x++)): return 1;
    default: return 0;
  }
  return x;
}


int h1(int x) {return x;}
int h2(int x) {return x;}
int h3(int x){return x;}

/* Issue gitlab #430. */
void main (void) {
  int s = sizeof( ((h1(1) && 1) || h2(1)) && h3(1) );
}
