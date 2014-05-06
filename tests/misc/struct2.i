/* run.config
  GCC:
  OPT: -val -deps -out -input  -main f_precis -journal-disable -absolute-valid-range 0x1000-0x2000
*/
struct st1 {
 int a;
 int *b;
};

struct st2 {
 int a;
 int d[10];
 struct st1 b;
 struct st1 e[10];
 struct st2 *c;
};

struct st1 tabst[10], tabst2[10];

struct st2 tab_s[2];
struct st2 tab_s1[2];
struct st2 tab_s2[2];
struct st2 tab_s3[2];
struct st2 tab_s4[2];

struct st2 s1,s2,s4,s5,s6;
struct st1 s8,s7;

typedef int Ttabl[5+5];
Ttabl tabl;

int tab1[2];
int tab2[2];
int tab3[2];
int tab4[2];
int tab5[2];
int tab6[2];

int *p, *p2, *p3, *p4, *p5, *p6, *p7;
int **q,**r,**s,**t;

int a,b; volatile int v;

void f_precis(int x, int i, int j, int k, int l, int m){

/* ---------------------------  */
/*          Scalaires           */
/* ---------------------------  */

  a                = i;

/* ---------------------------  */
/*         Structures           */
/* ---------------------------  */

  s1               = s2;

  s1.a             = x;

  s1.b.a           = x;

  s1.b             = s8;

  s7               = s6.b;

/* ---------------------------  */
/*         Tableaux             */
/* ---------------------------  */

  tab1[0]          = 2;

  tab1[1]          = 2;

  tab2[i]          = 2;

  tab3[i+j]        = k;

  tab4[tab2[i]]    = 2;

  tab5[tab2[1]]    = 2;

  tab6[tab2[i]+j]  = 2;


/* ---------------------------  */
/*   Tableaux  de structures    */
/* ---------------------------  */

  tab_s[0]           = s2;                              /* @tab_s[0...]        */

  tab_s[1].a         = x;

  tab_s1[i].b         = s8;                             /* @tab_s[?,b...]      */

  tab_s2[tabl[0]]     = s1;                             /* @tab_s[?...]        */

  tab_s3[tabl[1]].a   = x;

  tab_s4[tabl[i]+x].a = x;

/* ---------------------------  */
/*   Structures et tableaux     */
/* ---------------------------  */

  s1.d[1]          = x;

  s2.d[i]          = x;

/* ---------  */
/*  Pointeurs */
/* ---------  */

  p                = &a;

  *p               = x;

  *p               = *p + x;

  q = (int*)0x1000;
  r = (int*)0;

  *q               = p;

  if (v) **r              = a;

  p2               = &tab1[2];

  p3               = &tab1[i];

  if (v) b                = *(p3+2);

  p4               = p;

  p5               = (int *) 0x1000;

  p6               = (int*)0x1010;

  *p6              = *(int *) 0x1020 + i;

  p7               = p2 + 1;

/*  p8                = p2 - i; */

  s                = (int*)0x1030;

  *s               = (int *) 0x1040;

  t                = (int*)0x1050;
 (*t)[i]           = 2;

/* ---------  */

  s8.b             = &a;

  *(s8.b)          = x;

  s1.c             = &s2;

  s1.c->a          = x;

  s1.c->b          = s8;

  s1.c->b.a        = x;

  s1.c->b.b        = &a;

  *(s1.c->b.b)     = x;

  s1.c->c          = &s2;

  s1.c->c->a       = x;

  s1.c->c->b       = s8;

  s1.c->c->b.a     = x;

  s1.c->c->b.b     = &a;

  *(s1.c->c->b.b)  = x;

  s1.c->c->c       = &s2;

  s1.c->c->c->a    = x;

  s4.e[tabst[tab1[i+j]].a].a = *((char*)(tab2[k] + s5.e[tabst2[tab3[l] + m].a].b)+0x1060);

/*------------------------------*/
/* Clauses From attendues       */
/*------------------------------*/
/*  Clause From  :   @a[] From @i[*]; */
/*  Clause From  :   @s1[] From @s2[...]; */
/*  Clause From  :   @s1[a] From @x[*]; */
/*  Clause From  :   @s1[b,a] From @x[*]; */
/*  Clause From  :   @s1[b] From @s8[...]; */
/*  Clause From  :   @s7 From @s6[b...]; */
/*  Clause From  :   @tab1[(0)] From ; */
/*  Clause From  :   @tab1[(1)] From ; */
/*  Clause From  :   @tab2[(?)] From @i[*],@tab2[(?)]; */
/*  Clause From  :   @tab3[(?)] From @i[*],@j[*],@k[*],@tab3[(?)]; */
/*  Clause From  :   @tab4[(?)] From @tab2[(?),*],@i[*],@tab4[(?)]; */
/*  Clause From  :   @tab5[(?)] From @tab2[(2),*],@tab5[(?)]; */
/*  Clause From  :   @tab6[(?)] From @tab2[(?),*],@i[*],@j[*],@tab6[(?)]; */
/*  Clause From  :   @tab_s[(0)] From @s2[...]; */
/*  Clause From  :   @tab_s[(1),a] From @x[*]; */
/*  Clause From  :   @tab_s1[(?),b] From @i[*],@s8[...],@tab_s1[(?)]; */
/*  Clause From  :   @tab_s2[(?)] From @tabl[(0),*],@s1[...],@tab_s2[(?)]; */
/*  Clause From  :   @tab_s3[(?),a] From @tabl[(1),*],@x[*],@tab_s3[(?)]; */
/*  Clause From  :   @tab_s4[(?),a] From @tabl[(?),*],@i[*],@x[*],@x[*],@tab_s4[(?)]; */
/*  Clause From  :   @s1[d,(1)] From @x[*]; */
/*  Clause From  :   @s2[d,(?)] From @i[*],@x[*],@s2[d,(?)]; */
/*  Clause From  :   @p[] From @a[]; */
/*  Clause From  :   @p[*] From @x[*]; */
/*  Clause From  :   @p[*] From @p[*][*],@x[*]; */
/*  Clause From  :   @q[*] From @p[*]; */
/*  Clause From  :   @r[*][*] From @a[*]; */
/*  Clause From  :   @p2[] From @tab1[(2)]; */
/*  Clause From  :   @p3[] From @tab1[(?)],@i[*]; */
/*  Clause From  :   @p4[] From @p[*]; */
/*  Clause From  :   @p5[] From @Pt!4096[*]; */
/*  Clause From  :   @p6[*] From @Pt!4096[*][*],@i[*]; */
/*  Clause From  :   @s[*] From @Pt!4096[*]; */
/*  Clause From  :   @t[*][*][(?)] From @i[*],@t[*][*][(?)]; */
/*  Clause From  :   @s8[b] From @a[]; */
/*  Clause From  :   @s8[b,*] From @x[*]; */
/*  Clause From  :   @s1[c] From @s2[]; */
/*  Clause From  :   @s1[c,*][a] From @x[*]; */
/*  Clause From  :   @s1[c,*][b] From @s8[...]; */
/*  Clause From  :   @s1[c,*][b,a] From @x[*]; */
/*  Clause From  :   @s1[c,*][b,b] From @a[]; */
/*  Clause From  :   @s1[c,*][b,b,*] From @x[*]; */
/*  Clause From  :   @s1[c,*][c] From @s2[]; */
/*  Clause From  :   @s1[c,*][c,*][a] From @x[*]; */
/*  Clause From  :   @s1[c,*][c,*][b] From @s8[...]; */
/*  Clause From  :   @s1[c,*][c,*][b,a] From @x[*]; */
/*  Clause From  :   @s1[c,*][c,*][b,b] From @a[]; */

/*  Clause From  :   @s1[c,*][c,*][b,b,*] From @x[*]; */
/*  Clause From  :   @s1[c,*][c,*][c] From @s2[]; */
/*  Clause From  :   @s1[c,*][c,*][c,*][a] From @x[*]; */
/*  Clause From  :   @s4[e,(?),a] From   */
/*     @tabst[(?),a,*],@tab1[(?),*],@i[*],@j[*],@s5[e,(?),b,*][(?),*], */
/*     @tab2[(?),*],@k[*],@tabst2[(?),a,*],@tab3[(?),*],@l[*],@m[*],@s4[e,(?)]; */


}


static void fonc (int * p, int x) {
  *(p+3) = *p + x;
}

int Tab[10];
int * P;

void f_tab_0 (int y) {
  fonc (Tab, y);
}
void f_tab_2 (int y) {
  fonc (Tab+2, y);
}
void f_p_0 (int y) {
  fonc (P, y);
}
void f_p_2 (int y) {
  fonc (P+2, y);
}

void g (int * p) {
  *p = *p+1;
}
int test_g (void) {
  int x = 3;
  g (&x);
  return x;
}
