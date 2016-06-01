/* run.config*
   STDOPT: #"-machdep x86_32"
   STDOPT: #"-machdep ppc_32"
*/
int G= 77;
int GG;

struct A { int x; int y; };
struct B { int z; int t; };

struct A t[4];
struct A tt[5];

int g(struct A s)
{
  Frama_C_show_each_G(s);
  return s.y; // (*((struct B*)(&t[1]))).t;
  
}

struct A create_A() {
  struct A r={0,0};
  r.x = 1;
//  r.y = 2;
  Frama_C_show_each_GG(r);
  return r;
}

int main1(void)
{
  int i = 2 - 1;
  t[1].y = G;
  GG = g(tt[i]);
  struct A init = create_A();
  return g(t[i]);
}

struct CC { short c1; char * c2; };
extern struct CC C;

void h(struct CC c) {
  Frama_C_show_each(c.c1, c.c2);
}

void main() {
  main1();
  h(C);
}
