/* run.config
  GCC:
  STDOPT: #"-main main0"
  STDOPT: #"-main main1"
  STDOPT: #"-main main2"
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
