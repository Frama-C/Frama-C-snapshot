extern unsigned short t[100000];

int f();

void main(int i) {
  unsigned short *p = &t[i];
  int s = f(*p);
}
