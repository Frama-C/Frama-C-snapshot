extern int T1;
extern const int T2;

extern int T3[];
extern const int T4[];

extern int T5[3];
extern const int T6[3];
volatile int c;
void main () {
//  T1++;
//  T2++;
  if (c) T1= T3[3];
  if (c) T2= T4[3];
  T1= T5[1];
  T2= T6[1];

}
