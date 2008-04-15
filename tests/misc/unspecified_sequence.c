/* run.config
   STDOPT:
   STDOPT: +"-no-unspecified-access"
*/
/* detection of undefined behavior for read/write accesses
   in unspecified order
*/
int G[10];

int f (int x) { return x+1;}
int g (int x) { return x+2;}

int main () {
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
  int (*my_f) (int) = f;
  return (my_f=g, f(1)) + my_f(2);
}
