/* run.config
   OPT: -rte -warn-signed-overflow -print -then -rte-trivial-annotations
   OPT: -rte -warn-signed-overflow -print -unsafe-arrays
*/

int t[10];
int u[8+3];
int v[16][17];

typedef struct _s {
  int t[15];
  struct {
    int u[12];
  } s;
  struct _s v[12];
} ts;

ts s;

unsigned int c[10];

void main(int i, int j, unsigned int k) {
  t[0] = 0;
  u[1] = 0;
  v[2][3] = 0;
  s.t[1] = 0;
  s.s.u[2] = 0;
  s.v[3].t[4] = 0;

  t[i] = 0;
  u[i] = 0;
  v[i][j] = 0;
  s.t[i] = 0;
  s.s.u[i] = 0;
  s.v[i].t[j] = 0;

  t[k] = 0;
  u[k] = 0;
  v[k][c[k]] = 0;
  s.t[k] = 0;
  s.s.u[k] = 0;
  s.v[k].t[c[k]] = 0;
}
