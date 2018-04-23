extern int T1;
extern const int T2;

int* pT2 = (int *) &T2;

extern int T3[];
extern const int T4[];

extern int T5[3];
extern const int T6[3];
volatile int c;

extern struct fma {
  char nb;
  int t[];
} s;

void main () {
  if (c) T1= T3[3];
  if (c) *pT2= T4[3];
  T1= T5[1];
  *pT2= T6[1];
  s.nb = 1;
}
