/* run.config*
   STDOPT: #" -eva -calldeps "
 */

#include "string.h"


volatile v;

int x;

short t[50];

int u[12];

float f, fnan;
double d, dnan;

struct s {
  short s;
  int i;
  char c;
};

struct s w1[4];
struct s w2[4];

struct _bitf {
  int i1 : 3;
  unsigned int i2 : 3;
  char i3;
  unsigned int i4: 7;
  unsigned int i5: 8;
  char i6;
  signed int i7: 1;
  unsigned int i8: 1;
  signed int i9: 1;
  signed int i10: 1;
  unsigned int i11: 1;
  unsigned int i12: 1;
  signed int i13: 1;
  unsigned int i14: 1;
};

struct _bitf bitf;

int tone[1];

struct sub {
  short c1;
  int t[10];
  char c2;
};

struct sub vs;

struct structstruct {
  struct s c3;
  int c4;
};

struct structstruct vv;

typedef unsigned char BYTE;

#define SIZE 0x400

char t_b[SIZE];
short t_s[SIZE];

void main() {
  memset (&x, 0x2, sizeof(x));
  memset (&t, 0x3, sizeof(t));
  memset (&u, 154, sizeof(u));
  memset (&f, 0x2, sizeof(float));
  memset (&fnan, 0xFF, sizeof(float));
  memset (&d, 0x2, sizeof(double));
  memset (&dnan, 0xFF, sizeof(double));
  memset (&w1, 0x2, sizeof(w1));
  memset (&bitf, 126, sizeof(bitf));
  memset (&tone, 0x6, sizeof(tone));

  int n = v;
  //@ assert 1 <= n <= 8;
  memset (&w2, n, sizeof(w2));

  memset(&vs, 0x04, sizeof(short));
  memset(vs.t, 0x02, sizeof(vs.t));
  memset(&vv, 0x06, sizeof(short));
  memset(&t[15], 0x07, 20*sizeof(short));
  memset(t_b, 0x05, sizeof(t_b));
  memset(t_s, 0x13, sizeof(t_s));
}
