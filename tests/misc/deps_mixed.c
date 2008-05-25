int *p,*q;
int a,b;
int r=2;
int main (int c, int d) {
  p=c?&a:(int*)3;
  q=d?&b:(int*)2;
  r = *((p+ (int)q));
  return ((int)(p+ (int)q));
}
