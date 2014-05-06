/* run.config
   GCC:
   OPT: -val -deps -out -input -main f -journal-disable
   OPT: -val -deps -out -input -main fonc1 -journal-disable
   OPT: -val -deps -out -input -main fonc2 -journal-disable
*/
int f (int a, int b,int c){
  int w,d;

  if (c) b = 0;
  return w;
}

int fonc1 (int a, int b){
  int w;
  struct t1 { int x; int y;} v1;
  v1.x = a+b;
  w =  v1.x;
  if (a) {
    struct t1 { int x; int y;} v2;
    struct t2 { int x; int y;} v3;
    v2.x = a;
    v3.x = b;
    w = w + v2.x + v3.x;
    }
  return w;
}

int fonc2 (int a, int b){
  int w;
  struct t1 { int x; int y;} v1;
  v1.x = a+b;
  w =  v1.x;
  return w;
}

struct Tstr { int a; int b; };

int h (struct Tstr * ps) {
  return ps->a;
}
int ptr (int*pt) {
  return *pt;
}
int i (int x, int y) {
  struct Tstr s;// = {x, y};
  int g;
  g=0;
  return (*(&g));
  return ptr(&g);
  s.a = 0;
  return h(&s);
}
