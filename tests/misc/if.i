/* run.config
   GCC:
   OPT: -val -deps -out -input -main main -journal-disable
   OPT: -val -deps -main main6 -journal-disable
   */
int G,H,J;
int *p, *q;
int t[100];

int main0(void) {
  G=0;
  int c = 0;
  if (c) G=1; else G=2;

  return c;
}

int main1(void)
{
  if (G) ;

  return 1;
}

int main2(void) {
  int c = c?0:(c?1:2);
  int d = c?1:(c?2:3);

  G = -20;
  H = -30;

  if (c) {G=c; H=d;};

//  if (d>c) G=3; else G=4;

//  if (!(d<=c)) G=3; else G=4;

  return c;
}


int main3(void){
  G=0;
  H=1;
  p = &G;
  q = &H;

//  if (p==q) *p=2;

  return *q;
}

int main4(void) {
  int e1,e2;
  int c = e1?0:((e2)?1:2);
  int d = e1?1:((e2)?2:3);

  G = 20;
  H = 30;

  if (d<c) {G=d; H=c; } else G=4;

//  if (!(d<=c)) G=3; else G=4;

  return c;
}

void main(void)
{
  q = t;
  p = t + G;
  if ((p >= &t[10]) && (p <= &t[99]))
     q = p;
}

void def(void)
{
  if (J)
    G = H;
}

int main5(void)
{
	G = 0;
	  if (G) H=J;

	    return 1;
}

int main6(int c, int d)
{
  G = 0;
  if (G) if (d) G=2; else G = 1;
  // G isn't modified
  return 1;
}
