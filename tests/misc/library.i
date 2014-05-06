/* run.config
   GCC:
   OPT: -val -deps -out -input -lib-entry -main main -context-depth 3 -journal-disable -then -main main2 -then -context-width 4
*/
int f_int(int x);
int *f_star_int(int x);

int ****G;
int G0,*G1;

typedef int (*pfun)(int *p1, const int *p2);
pfun gen();
extern pfun f;
float *i(); double *k();

void main(pfun g) {
  G0 = f_int(2);
  G1 = f_star_int(5);
  *G1 = 5;
  ****G=1;

  int x = 3;
  int y = 4;
  pfun h = gen();
  int z1 = f(&x, &y);
  int z2 = g(&x, &y);
  int z3 = h(&x, &y);
  float *pf = i();
  float vf = *pf;
  *pf = 1.;
  *pf += *pf;
  double *pd = k();
  *pd = 2.;
}

struct {
  void (*f[2])();
} s;


struct {
  struct ss *p[8];
  struct ss (*q)[8];
} ss;

void (*ff)();


void main2(){}
