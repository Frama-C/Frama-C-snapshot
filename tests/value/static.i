int * f (void)
{
  static int x;
  return &x;
}

int GLOB={{{{0}}}};
char T[10]={0};
int IT[10]={0};
int G;
char H;
int R;

int volatile *p;
int a[2]={77};
int Rv=99;

int main() {
  *(f()) = 3;
  R = *f();
  GLOB = sizeof main ();
  G = *((int*)&(T[1]));
  H = *((char*)&(IT[9]));


  p = &a;
  Rv = *p;

  return T[0];

}
  
