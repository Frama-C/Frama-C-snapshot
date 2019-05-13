#define t Frama_C_periodic_t_320


int g[10] __attribute__ ((Frama_C_periodic)); // garbled

typedef struct {
  short s1;
  short s2;
  short s3;
} ts;

int t[60]= {1};
int (u __attribute__ ((Frama_C_periodic)))[60]= {-1,-2,-3};
int v[3] __attribute__ ((Frama_C_periodic)) = {-1,-2,-3};
ts w[10] __attribute__ ((Frama_C_periodic));


int Au,Bu,Cu,Du,Eu,Fu,Gu = 12, Hu;
int At,Bt,Ct,Dt,Et,Ft,Gt = 12, Ht;

void main()
{
  At = t[0];
  Bt = t[11];
  Ft = 2 * (t[20] + 1);
  t[13] = Ft;
  Ct = t[2];
  t[41] = 3 * Ft;
  Et = t[12];
  t[4] = 2 * Gt;
  Ht = 2 * t[25] + 1;

  Au = u[0];
  Bu = u[11];
  Fu = 2 * (u[22] + 1);
  u[13] = Fu;
  Cu = u[2];
  u[41] = 3 * Fu;
  Eu = u[12];
  u[4] = 2 * Gu;
  Hu = 2 * u[25] + 1;

  v[1] = 1;

  w[0].s1 = 1;
  w[1].s2 = 2;
  w[2].s3 = w[0].s1 + w[1].s2;

  Frama_C_dump_each();

  int *p = (&g + (int)&g) - (int)&g; // creates a garbled mix
  *p = 1;
  p = (&g + (int)&g) - (int)&g; // creates a garbled mix
  int vg = *p;
  *p = (int) &vg;
}
