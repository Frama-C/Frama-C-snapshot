/* run.config
   GCC:
   OPT: -val -lib-entry -main f -absolute-valid-range 0x200-0x199 -journal-disable
*/


int a,b,c;

int star_p, star_w___500;

struct str { int s1; int s2; int *sp ; int (*sg)(char *) ; } s;

struct str t;

struct strstr { struct str ss1; int ss2; } tt;

int u[12];
int v[12][3];
int *(w[12]);

struct str ts[10];

union uni { int u1 ; struct str u2 ; } uu ;

struct str_arith { int s1; int s2; float s3; } ;

union uni_arith { int u1 ; struct str_arith u2 ; float u3 ; } uuu ;

const int c_int = 34;

extern struct {
  int f1;
  void *p; // void* field: valid, size unknown
} svoid;

extern void *qvoid; // void* pointer: valid, size unknown


int f(int x, float y, int **p, int (*g)(char *), void *vv, void **vvv, int ta[5])
{
  if (x >= 0) a = x;
  b = s.s1 ;
  t.s2 = 3;
  tt.ss2 = c;
  p = p;
  *p = *p;
  u[1]=2;
  v[0][0]=5;
  w[4]=&a;
  (ts[3]).s1 =   (ts[3]).s1 ;
  vv = vv;
  *vvv = *vvv;
  c_int = c_int;
  uu.u1 = uu.u1;
  uuu.u1 = uuu.u1;
  ta[1]=3;
  ta=ta;
  char* pvoid = svoid.p;
  *pvoid = 1;
  pvoid = qvoid;
  *pvoid = &pvoid;
  return g("toto");

}
