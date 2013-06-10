/* run.config
   OPT: -rte -warn-signed-overflow -print -machdep x86_32 -then -unsafe-arrays
*/

struct R {
  int v;
};

struct Q {
  int v;
  int id[12];
  struct P* next;
  struct R tr[13];
};

struct P {
  int val;
  struct P* next;
  int id[5];
  int oth[6][7];
  struct P* nexts[8][9];
  struct Q q;
  struct Q tq[10][11];
  struct P*** znexts;
};

int main() {
  struct P*** pppp;
  struct P** ppp;
  struct P* pp;
  struct P p, p2;
  int v;
  struct Q q;
  int *i,j;
  int i0 = 0;
  int i1 = 1;
  int i2 = 2;
  int i3 = 3;
  int i4 = 4;
  j=0;
  i = &j;
  pp = &p;
  ppp = &pp;
  pppp = &ppp;
  p.next = pp;
  p.znexts = pppp;
  p.nexts[i0][i1] = p.nexts[i2][i3] = pp;
  q.next = pp;
  q.id[i0] = 0;
  p.q = q;
  p.tq[i0][i1] = q;
  p.id[i1] = p.id[i2];
  p.id[i1] = p.id[i3];

  struct P np = *(p.next);
  struct P* npp = p.next;

  v = p.id[3];
  v = pp->id[3];

  
  v = *i;
  v = pp->val;
  v = pp->id[3];
  struct P* z = pp->nexts[i0][i1];

  v = pp->nexts[i0][i1]->val;
  v = pp->next->val;
  v = pp->next->next->val;
  v = p.nexts[i0][i1]->val;
  v = pp->id[i2];
  v = pp ->oth[i0][i1];
  v = p.nexts[i1][i2]->nexts[i3][*i]->id[i4];
  v = p.q.v;
  v = p.q.id[i4];
  v = p.tq[i3][i1].v;
  v = p.tq[i1][i2].next->tq[i3][i4].v;
  v = pp->tq[i3][i1].v;

  v = p.znexts[i0][i1][i2].val;

  q = p.tq[i0][i1];

  v = p.tq[i0][i1].tr[i2].v;

  v = pp->val;
  v = p.val;
  v = p.tq[i0][i1].v;

  return v;
}
