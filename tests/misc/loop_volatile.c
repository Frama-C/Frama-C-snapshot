int volatile *p;
int a[2]={77};
int R=99;

void main () {
  p = &a;
  R = *p;
}
