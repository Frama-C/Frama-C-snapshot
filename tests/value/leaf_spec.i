/* run.config*
   STDOPT: #"-main main"
   STDOPT: #"-main main1"
*/

void f(int * x, int * y, int **z, int a, char b);


void f1(int y);

int g(int x);

int *h(int y);

int *k( int *l);
int *k0( int const *l);

void main () {
  f1(0);
  g(2);
  h(0);
  k(0);k0(0);
}

void main1(void)
{
  f(0,0,0,0,0);
}
