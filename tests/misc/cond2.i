/* run.config
  GCC:
  OPT: -val -deps -out -out-external -input  -main zero_ou_un_0 -journal-disable
  OPT: -val -deps -out -out-external -input  -main un_1 -journal-disable

*/

volatile int Gx;
volatile int Gy;
int *px,*py,x,y;
int T[100]={0};
  int r = 0;
  int s = 0;
  int t = 0;
  int u = 0;

void zero_ou_un_0 (void) {
  int i ;

  x = Gx ? 0 : 2 ; ;
  if (x != 0)
    r = 1;

  x = 1;
  y = 0;
  for (i = 0 ; i < Gx ; i++) x += 2;
  for (i = 0 ; i < Gy ; i++) y += 5;
  if (x != y)
    s = 1;

  x = Gx ? 0 : 2 ; ;
  y = Gy ? 1 : 2 ; ;
  if (x != y)
      t = 1;

  x = Gx ? 0 : 2 ; ;
  if (x != 1)
      u = 1;

}

int un_1 (void) {
  int r = 0;
  int i ;
  x = Gx ? 0 : 2 ; ;
  y = Gy ? 1 : 3 ; ;
  if (x != y)
      r = 1;

  x = 1;
  y = 0;
  for (i = 0 ; i < Gx ; i++) x += 2;
  for (i = 0 ; i < Gy ; i++) y += 2;
  if (x != y)
      s = 1;

  x = Gx ? 0 : 2;
  y = Gy ? 1 : 3;
  for (i = 0 ; i < Gx ; i++) x += 4;
  for (i = 0 ; i < Gy ; i++) y += 4;
  if (x != y)
      t = 1;

  px = Gx ? &(T[0]) : &(T[0]);
  py = Gy ? &(T[1]) : &(T[1]);
  for (i = 0 ; i < Gx ; i++) {px += 4; *px=1; }
  for (i = 0 ; i < Gy ; i++) {py += 4; *py=2; }
  if (px != py)
      u = 1;
  return u;
}
