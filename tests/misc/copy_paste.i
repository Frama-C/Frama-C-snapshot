int t[12],G;

void main(int c) {
  volatile int l=0;
  int i=c?3:4;
  int j=c?(-3):4;
  t[i] = i;
  t[j] = j;
  if (l) l = *(int*)l;
  G=l;
}
