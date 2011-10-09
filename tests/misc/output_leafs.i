
int *H,G,K,L,M,N,P;

/*@ assigns G \from G,*H;
  @ assigns *H \from P;
  @ assigns *x \from \empty;
*/
void crypt(int*x);

void main1(int y)
{
  H = &K;
  crypt(&L);

}

int a, b, c, d;

//@ assigns *u \from *v;
void g(int *v, const int *u);

void g1() {
  g(&a,&b);
}

void g2() {
  g(&c,&d);
}

void main2 () {
  g1();
  g2();
}


void f(int* x);

int main3 () {
  int x = 0; 
  f(&x);
  return x; 
}

void main(int y) {
  main1(y);
  main2();
  main3();
}
