/* run.config*
   STDOPT: +"-print -inout"
*/
volatile int v;
int G;

//@ assigns s[..] \from s[..];
void F1(char *s);

char T[100];
char Tpost[100];

typedef struct {
  int f1;
  int f2;
} ts;

ts t[10];
int t2[100000];
int t3[100000];

//@ assigns *(p+(0..3)) \from *(p+(4..7));
void f (char *p);

//@ assigns t2[((unsigned char)len)+1 .. ((unsigned char)len)+1] \from \nothing;
void g(int len);

//@ assigns p[..] \from \nothing;
void h(int *p);
/*@ type Lint = int; */
/*@ logic Lint foo(Lint p) ; */
//@ assigns p[0..foo(*p)] \from \nothing;
void j(int *p);

int x;
int k = 53;

/*@ assigns x \from \at(x, Post);
    assigns Tpost[\at(i, Post)];
    assigns Tpost[\at(k, Post)];
*/
void assigns_post(int i);

void main1(void)
{
  F1(T);

  for (int i=0;i<=5;i++)
    f((char*)&t[i].f2);

  g(2 * (int)(&T) );
  h((int*)(2 * (int)(&t3)));
  
  j((int*)(T+9));

  assigns_post(18);
}

//@ assigns \result;
int ff1(void);

int* ff2(void);
//@ assigns \nothing;
int* ff2_bis(void);

int y1, y2, y3;

/*@ assigns y1, y2, y3; assigns y2 \from y2;*/
void ff3(void);

void ff4(void);

int ff5(void);

int main2() {
  int l = ff1();

  ff3(); // warn for absence of \from for y1 and y3
  ff4(); // No warning, result has type void
  ff5(); // No warning, result is unused

  int *p = ff2(); // warn on missing assigns \result
  int *q = ff2_bis(); // make sure to return NULL in the result
  if (p != &x)
    return 1;
  return 0;
}

int t_main3_1[7][8];
int t_main3_2[3][4][5];

int main3(int a[][8], int b[3][4][5]);

ts t_main4[1000];
ts u_main4[100];

//@ assigns t_main4[i].f1 \from \nothing; assigns u_main4[i].f1 \from \nothing;
void f_main4_1(int i);

//@ assigns t_main4[0..999].f1 \from \nothing; assigns u_main4[0..99].f1 \from \nothing;
void f_main4_2(void);

void main4() {
  f_main4_1(v);
  f_main4_2();
}


void main() {
  main1();
  main2();
  main3(t_main3_1, t_main3_2);
  main4();
}
