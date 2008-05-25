struct t1 { unsigned int a:2; int b:4; int c:22;} h;
struct t2 { unsigned int a:2; int b:4; int c:22; int d;} k;

unsigned int VV=55;
int main (int a, int b){
  struct t1 v,w;
  VV = h.a;

  h.a = VV;

  v.c = &v;
  v.a = 4;
  v.b = 7;
  h.b = a+b + h.a + h.b;
  h.c = &v +1;
}
