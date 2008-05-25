int G;
void main() {
  volatile int a = 0, b = 0, tmp = 0;
  if (a > b) { tmp = a; a = b; b = tmp; }
  /*@ assert  a <= b; */

}

void main1() {
  volatile int a=0,b=0,c=0,d=0,tmp=0;

  if (a > b) { tmp = a; a = b; b = tmp; }
  if (c > d) { tmp = c; c = d; d = tmp; }
  if (a > c) { tmp = a; a = c; c = tmp; }
  if (b > d) { tmp = b; b = d; d = tmp; }
  if (b > c) { tmp = b; b = c; c = tmp; }
  /*@ assert a <= b <= c <= d; */
}

void main2() {
  int a=0, b=0;
  L:if (a > b) { a += b; b = a-b; a -= b;}
  a = a * 2;
  goto L;
  /* assert  a <= 2*b;  */
  /* assert b == \at(a,L) || b == \at(b,L); */

}
