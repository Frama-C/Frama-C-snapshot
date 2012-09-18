int G;

//@ assigns s[..] \from s[..];
void F1(char *s);

char T[100];


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


void main(void)
{
  F1(T);

  for (int i=0;i<=5;i++)
    f(&t[i].f2);

  g(2 * (int)(&T) );
  h(2 * (int)(&t3) );

}
