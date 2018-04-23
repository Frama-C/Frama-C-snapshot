/*run.config*
STDOPT: #"-no-collapse-call-cast" +"-print"
STDOPT: #"-collapse-call-cast" +"-print"
*/

int f1(void);
long f2(void);
unsigned int f3(void);
float f4(void);
double f5(void);
int *f6(void);
void *f7(void);

void main2() {
  long r1 = f1();
  short r2 = f1();
  int r3 = f1();
  unsigned int r4 = f1();

  long r5 = f2();
  int r6 = f2();
  unsigned long r7 = f2();

  unsigned int r8 = f3();
  int r8bis = f3();

  float r9 = f4();
  double r10 = f4();

  float r11 = f5();
  double r12 = f5();

  void* r13 = f6();
  int* r14 = f6();
  char* r15 = f6();

  void* r16 = f7();
  int* r17 = f7();
}



int f(int x) { return x+1; }

void main () {
  short x = 4;
  x = f(42);

  main2();
}
