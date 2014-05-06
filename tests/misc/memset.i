/* run.config
   STDOPT: +"-calldeps" +"-no-deps" +"-inout-callwise" +"-inout" +"-value-msg-key imprecision" +"-plevel 500"
*/

//@ assigns *((char*)dst+(0..size-1)) \from v;
void Frama_C_memset(void * dst, int v, unsigned long size);

int t1[100];
int t2[100];
int t3[100];
int t4[100];
int t5[100];
int t6[100];
int t7[100];
int t8[100];
int t9[100];
int t10[100];
int t11[100];

struct s {
  char f1;
  short f2;
  int f3;
  int f4[3];
};

struct s ts[5];

volatile int vol;

void main() {
  Frama_C_memset(t1, 0x11, sizeof(t1)); // basic
  Frama_C_memset(t2+(int)t2, 0x12, sizeof(t2)); // garbled dest
  Frama_C_memset(t3+10, 0x11, t1); // garbled size

  if (vol) {
    Frama_C_memset(t4+1, 1, sizeof(t4)); // out of bounds
  }

  Frama_C_memset(t5, t1, sizeof(t4)); // garbled char

  int *p = vol ? t6+10 : t7;
  Frama_C_memset(p, 0x22, 16); // multiple dest

  p = vol ? (char*) 0 : t8;
  Frama_C_memset(p, 0x22, 16); // one valid dest; TODO

  p = t9+20;
  while (1) {
    if (vol) break;
    p++;
  }
  Frama_C_memset(p, 0x8FE, 4); // imprecise dest

  unsigned long s = 12;
  if (vol) s += 24;
  Frama_C_memset(t10+4, 0x88, s); // imprecise size

  unsigned long s1 = 8;
  if (vol) s1 += 8;
  p = t11 + 2;
  if (vol)
    p++;
  Frama_C_memset(p, 0x99, s1); // imprecise dest+size with juxtaposition

  if (vol)
    Frama_C_memset(ts, 254, sizeof(ts));
}
