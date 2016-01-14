/* run.config
   OPT: -wp-prop none -wp-log refusage
*/

/* run.config_qualif
   DONTRUN:
*/

int x0, y0, z0, *p0;
int x1=1, y1=1, z1=z0, *q0=(int*)0, *q1=&y0, *p1=&y1;

struct s { int c; int* cp; } ;

int a0, a1;
struct s s0, v0, w0;
struct s s1=s0;
struct s s2={1,(int*)0};
struct s s3={1,&a0};
struct s s4={a1,(int*)0};
struct s s5={1,&v0.c};
struct s s6={w0.c,(int*)0};


void f(void) {
  int lx0, ly0, lz0, *lp0;
  int lx1=1, ly1=1, lz1=lz0, *lq0=(int*)0, *lq1=&ly0, *lp1=&ly1;

  int la0, la1;
  struct s ls0, lv0, lw0;
  struct s ls1=ls0;
  struct s ls2={1,(int*)0};
  struct s ls3={1,&la0};
  struct s ls4={la1,(int*)0};
  struct s ls5={1,&lv0.c};
  struct s ls6={lw0.c,(int*)0};
}
