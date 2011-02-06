#define t Frama_C_periodic_t_320
#define u Frama_C_periodic_u_32
#define v Frama_C_periodic_v_32


int t[60]= {1};
int u[60]= {-1,-2,-3};
int v[3]= {-1,-2,-3};

int Au,Bu,Cu,Du,Eu,Fu,Gu = 12, Hu;
int At,Bt,Ct,Dt,Et,Ft,Gt = 12, Ht;

int main()
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

  Frama_C_dump_each();

  return 0;
}
