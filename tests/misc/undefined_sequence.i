/* run.config
   STDOPT: +"-unspecified-access"
   STDOPT: +"-no-unspecified-access"
*/
/* detection of undefined behavior for read/write accesses
   in unspecified order
*/
int G[10];

int f (int x) { return x+1;}
int g (int x) { return x+2;}

int r, H;
int h(int i)
{
  r = r++; //UB
  i = r++; //Not UB
  H = i;
  return i;
}

int func(int x, int y) { return x + y; }

int main (int a) {
  int x, *y, i,j;
  x = 0;
  y = &x;
  i=(x=0)+(*y=1);
  for (i=0; i < 10; i++) G[i] = i;
  i=j=0;
  while (j<10 && i<10) G[j++] = G[i++];
  i=j=0;
  while (j<10 && i<10) G[j++] += G[i++];
  i=j=0;
  while(j<10 && i<10) {
    G[j] = G[j++]; // UB
    G[i++] = G[i]; // UB
  }
  i=j=0;
  while(j<10 && i<10) {
    G[j] += G[j++]; // UB
    G[i++] += G[i]; // UB
  } 
  i = f(g(3)+x) + x++; //UB
  *y = f(g(3)+x); // no UB: x is read to write to x (through an alias)
  if (a)
    r = h(1) + h(2); // missing alarm!

  y = &G[2];

  *y =  (G[2] < (func((0U ||
         (((G[2] ^ G[2]) <= G[2]) < ((*y) || G[2]))), 5)));

  int (*my_f) (int) = f;
  return (my_f=g, f(1)) + my_f(2);
}
