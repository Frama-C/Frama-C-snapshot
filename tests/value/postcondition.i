int G;
int A,B,C,D,E,EX,X;

//@ ensures -100 <= \result <= 100 ;
int u(void);

//@ ensures min <= \result <= max ;
int cap(int min, int max);

/*@
  @ requires 0<=cmd<5;
  @ ensures  0<=\result<300; // Seems to be a false postcondition
  @*/
int get_index(int /* in */ cmd)
{
  int ret=0;
  Frama_C_show_each_cmd(cmd);
  while (ret <= 100*cmd)
    {
      if (u()) return ret;
      ret++;
    }
  return ret;
}

/*@ ensures EX <= cmd ; */
int bound(int cmd)
{
  cmd = 2;  /* vicious */
  return 0;
}

//@ ensures G == 6;
void t0 () {
  G = 6;
}

//@ ensures G == 7;
void t1 () {
  G = 6;
}

int *p;

//@ ensures *p == 6 && G == *p && G == 6;
void t2 () {
  p = &G;
  *p = 6;
}


typedef struct {
  int a;
  int b;
  int c;
} st;

st TAB[10];

//@ ensures TAB->a == 12;
void t3 () {
  TAB->a = 12;
}

//@ ensures x<=y;
void t4(int x, int y) {
  x++; y--;
  return;
}

/*@ ensures x == \old(x);
  ensures \result > \old(X); */
int t5(int x) {
  x = X;
  return ++x;
}

/*@ ensures \result == 0;
  @ ensures \false;
  @ */
int f(void) { return 0; }

void main(){
  B=get_index(1);
  EX = u();
  bound(8);
  C=get_index(u()?4:6);
  D = u();
  E = cap(20, 80);
  if (u()) t0();
  if (u()) t1();
  if (u()) t2();
  if (u()) t3();
  t4(3,4);
  if (u()) { X = 8; t5(2); }
  if (B) f();
}
