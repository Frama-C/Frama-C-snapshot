/* run.config
  GCC:
  OPT: -val -deps -out -input  -main main0 -journal-disable
  OPT: -val -deps -out -input  -main main1 -journal-disable
  OPT: -val -deps -out -input  -main main2 -journal-disable
*/

int X,c,u,v,w,G;

int incr(int* a,int* b) {
  (*a)++;
  (*b)++;
  return *a+*b;
}


int sum(int a,int b) {
  return a+b;
}

int G=0,H=0,I=0;
int main0 () {
  I=incr(&G,&H);
  return I;
}

int main1 () {
  I=incr(&G,&G);
  return I;
}

int main2() {
  I = sum(G,H);
  return I;
}
